# Currently manually downloading the powerpoint files
# But will likely need to source an R script to download them from SharePoint

from pptx import Presentation
from pptx.util import Pt, Cm
from pathlib import Path
import re
from copy import deepcopy
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
    "System Safety": {}
}

# Prepare PowerPoint for saving as images
Application = win32com.client.Dispatch("PowerPoint.Application")

# Define functions
def clean_whitespace(t):
    return re.sub(r"’", "'", re.sub(r"[\r\n\s\r\u2028\u2029]+", " ", t).strip())

def duplicate_slide(source_slide, target_prs):
    layout_name = source_slide.slide_layout.name

    if layout_name in [l.name for l in target_prs.slide_layouts]:
        target_layout = next(layout for layout in target_prs.slide_layouts if layout.name == layout_name)
    else:
        target_layout = target_prs.slide_layouts[6]

    new_slide = target_prs.slides.add_slide(target_layout)

    for shape in source_slide.shapes:
        el = deepcopy(shape.element)
        new_slide.shapes._spTree.insert_element_before(el, "p:extLst")
    return new_slide

# Read each PowerPoint and identify needed slides
for ppt_file in input_folder.glob("*.pptx"):
    hin_name = str(Path(ppt_file).stem).split("Qual")[0].rstrip("[_ ]").removesuffix("Master").rstrip("[_ ]")
    hin_groups = {k:[] for k in groups.keys()}
    print(f"Reading {hin_name} PPTX file")
    prs = Presentation(ppt_file)
    current_group = None
    slides_to_save = []
    for i, slide in enumerate(prs.slides, start=1):
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
    hin_folder = f"{input_folder.resolve()}/{re.sub(' ', '_', hin_name.lower())}"
    os.makedirs(hin_folder, exist_ok=True)

    pptx = Application.Presentations.Open(str(ppt_file.resolve()),
                                          ReadOnly=True,
                                          WithWindow=False)
    for i in slides_to_save:
        pptx.Slides[i-1].Export(f"{hin_folder}/Slide{i}.PNG", "PNG")
    pptx.Close()
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
        for section_slide_number, original_slide_number in enumerate(groups[group_name][hin], start=1):
            img_path = f"{input_folder}/{re.sub(' ', '_', hin.lower())}/Slide{original_slide_number}.PNG"

            with Image.open(img_path) as img:
                img_width_px, img_height_px = img.size
            avail_width = prs.slide_width - 2*margin
            avail_height = prs.slide_height - margin

            scale = min(avail_width / img_width_px,
                        avail_height / img_height_px)

            img_width = int(img_width_px * scale)
            img_height = int(img_height_px * scale)

            left = int((prs.slide_width - img_width) / 2)

            slide = prs.slides.add_slide(prs.slide_layouts[6])
            slide.shapes.add_picture(img_path, left=left, top=margin,
                                    width=img_width, height=img_height)

            textbox = slide.shapes.add_textbox(left=margin/2, top=0,
                                            width=Cm(15), height=margin)
            p = textbox.text_frame.paragraphs[0]
            p.text = f"{group_name} - {hin} - Slide {section_slide_number}"
            p.font.size = Pt(24)
            p.font.bold = True
        HIN_num_slides.update({hin: section_slide_number})

    prs.save(f"output/powerpoints/{group_name}.pptx")

    print(f"{group_name} slide deck created with slides from {num_HINs} HINs. Slide number details:")
    for hin in HIN_num_slides.keys():
        print(f" - {hin}: {HIN_num_slides[hin]} slides")

# Add summary info boxes
# Save to SharePoint