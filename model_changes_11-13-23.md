### bird-human model changes v9 ###
added a restriction to bird breeding, now 50% chance regulated by bird-density of the patch.
    Might be too strong?
    Line 153/154

Tested median bird-density of patches instead of mean..
    Leads to too many 0's and patches increase bird density too much
    Line 232

Decided to regulate the bird estimate values by dividing by 4
    Line 215

Changed landscape center to bottom left corner

Notes:
    seems that when average bird density of landscape becomes very high,
    bird love will start to decrease in tiny pockets where bird estimates are lower

    s