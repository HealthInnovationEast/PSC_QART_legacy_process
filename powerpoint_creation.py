from pptx import Presentation
from pptx.util import Pt, Cm
from pptx.enum.dml import MSO_COLOR_TYPE
from pathlib import Path
import re
import win32com.client
import os
from PIL import Image

input_folder = Path("data/powerpoints/")
header_slide_layouts = {"Front title slide": 0,
                        "8_Breaker Heading1-Blue-DarkBlueA": 14}
end_slide_layout = "End Slide ACCESSIBLE"
template_file = "ppt_template.pptx"

groups = {
    "Martha's Rule": {},
    "Medicines Safety": {},
    "Maternity and Neonatal Safety": {},
    "System Safety": {},
    "Overview": {}
}

programmes = [g for g in groups.keys() if not g == "Overview"]

# Prepare PowerPoint for saving as images
Application = win32com.client.Dispatch("PowerPoint.Application")
os.makedirs(f"{input_folder}/programme_slide/", exist_ok=True)

# Define functions
def clean_whitespace(t):
    return re.sub(r"’", "'", re.sub(r"[\r\n\s\r\u2028\u2029]+", " ", t).strip())

# Read each PowerPoint and identify needed slides
for ppt_file in input_folder.glob("*.pptx"):
    hin_name = str(Path(ppt_file).stem).split("Qual")[0].rstrip("[_ ]").removesuffix("Master").rstrip("[_ ]")
    hin_groups = {k:[] for k in groups.keys()}
    print(f"Reading {hin_name} PPTX file")
    prs = Presentation(ppt_file)
    current_group = None
    slides_to_save = []
    for i, slide in enumerate(prs.slides, start=1):
        if i==2:
            hin_groups["Overview"] += [i]
            slides_to_save += [i]
            continue

        if slide.slide_layout.name == end_slide_layout:
            break

        if slide.slide_layout.name in header_slide_layouts.keys():
            header_placeholder_idx = header_slide_layouts[slide.slide_layout.name]
            try:
                title = clean_whitespace(slide.placeholders[header_placeholder_idx].text)
            except:
                title = None

            if title in groups:
                current_group = title
            else:
                current_group = None
        else:
            if current_group:
                hin_groups[current_group] += [i]
                slides_to_save += [i]
    for g, values in hin_groups.items():
        groups[g].update({hin_name: values})

    # Also, save the pptx as images
    hin_folder = f"{input_folder.resolve()}/{hin_name}"
    os.makedirs(hin_folder, exist_ok=True)

    pptx = Application.Presentations.Open(str(ppt_file.resolve()),
                                          ReadOnly=True,
                                          WithWindow=False)
    for i in slides_to_save:
        pptx.Slides[i-1].Export(f"{hin_folder}/Slide{i}.PNG", "PNG")

    # Create four new slide decks for each HIN for creating the summary slides
    # Delete all slides except slide 2 (summary slide)
    for i in range(pptx.Slides.Count, 0, -1):
        if i != 2:
            pptx.Slides(i).Delete()

    # Save the new decks
    for p in programmes:
        pptx.SaveAs(f"{input_folder.resolve()}/programme_slide/{hin_name}__{p}.pptx")
    print(f"{hin_name} programme slide files created")
    pptx.Close()

# Prepare each programme summary slide
for p_slide in (input_folder/"programme_slide").glob("*.pptx"):
    hin = p_slide.name.split("__")[0]
    programme = p_slide.name.split("__")[1].removesuffix(".pptx")
    prs = Presentation(p_slide)
    slide = prs.slides[0]

    colour_type = None
    line_colour = None

    # Get colour of programme
    for shape in slide.shapes:
        if not hasattr(shape, "text"):
            continue
        clean_text = re.sub(" safety", "", clean_whitespace(shape.text.strip()).lower())
    
        if re.sub(" safety", "", programme.lower()) == clean_text:
            colour_type = shape.line.color.type
            if colour_type == MSO_COLOR_TYPE.RGB:
                line_colour = shape.line.color.rgb
            elif colour_type == MSO_COLOR_TYPE.SCHEME:
                line_colour = shape.line.color.theme_color
            else:
                line_colour = None

    # Remove shapes that don't match the colour
    for shape in slide.shapes:
        colour_match = False
        if hasattr(shape, "text"):
            if "Highlight Report" in shape.text:
                colour_match = True
            elif "Milestones status" in shape.text:
                colour_match = True
        else:
            colour_match = True
        if (not colour_match) and colour_type and line_colour:
            if colour_type == MSO_COLOR_TYPE.RGB:
                try:
                    if shape.line.color.rgb == line_colour:
                        colour_match = True
                except:
                    pass
            elif colour_type == MSO_COLOR_TYPE.SCHEME:
                try:
                    if shape.line.color.theme_color == line_colour:
                        colour_match = True
                except:
                    pass

        # Delete if no match
        if not colour_match:
            shape.element.getparent().remove(shape.element)

    # Resave slide
    prs.save(p_slide)

    # Save as image
    pptx = Application.Presentations.Open(str(p_slide.resolve()),
                                              ReadOnly=True,
                                              WithWindow=False)
    pptx.Slides[0].Export(re.sub("pptx", "PNG", str(p_slide.resolve())), "PNG")
    pptx.Close()
    print(f"Image created: {hin} programme slide for {programme}")

Application.Quit()

margin = Cm(2.5)
# Create the new slides
for group_name in groups.keys():
    prs = Presentation(template_file)
    num_HINs = 0
    HIN_num_slides = {}
    for hin in groups[group_name].keys():
        num_HINs += 1
        slide_section_number = 0

        # Define image slides to add
        if group_name in programmes:
            image_slides = [{"image_file": f"{input_folder}/programme_slide/{hin}__{group_name}.PNG",
                            "image_title": f"{group_name} - {hin} -\nExcerpt from Highlight Report slide"}]
        else:
            image_slides = []

        for section_slide_number, original_slide_number in enumerate(groups[group_name][hin], start=1):
            img_path = f"{input_folder}/{hin}/Slide{original_slide_number}.PNG"
            slide_detail = "Highlight Report" if group_name == "Overview" else f"Slide {section_slide_number}"
            img_title = f"{group_name} - {hin} - {slide_detail}"
            image_slides.append({"image_file": img_path,
                                 "image_title": img_title})

        # Add the images to slides
        for i_slide in image_slides:
            with Image.open(i_slide["image_file"]) as img:
                img_width_px, img_height_px = img.size
            avail_width = prs.slide_width - 2*margin
            avail_height = prs.slide_height - margin

            scale = min(avail_width / img_width_px,
                        avail_height / img_height_px)

            img_width = int(img_width_px * scale)
            img_height = int(img_height_px * scale)

            left = int((prs.slide_width - img_width) / 2)

            slide = prs.slides.add_slide(prs.slide_layouts[6])
            slide.shapes.add_picture(i_slide["image_file"], left=left, top=margin,
                                     width=img_width, height=img_height)

            textbox = slide.shapes.add_textbox(left=margin/2, top=0,
                                            width=Cm(15), height=margin)
            p = textbox.text_frame.paragraphs[0]
            p.text = i_slide["image_title"]
            p.font.size = Pt(24)
            p.font.bold = True
        HIN_num_slides.update({hin: section_slide_number})

    prs.save(f"output/powerpoints/{group_name}.pptx")

    print(f"{group_name} slide deck created with slides from {num_HINs} HINs. Slide number details:")
    for hin in HIN_num_slides.keys():
        print(f" - {hin}: {HIN_num_slides[hin]} slides")
