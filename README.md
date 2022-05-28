# elm-calc : Template Code For Algodynamics Intern 2022

Live Demo : [Link](https://archit.goyal.gitlab.io/elm-calc/index.html)

## Objectives

1. Learn about SVG coordinate system
2. Learn about SVG attributes
3. Learn about Elm Package API designs
4. Learn about code organisation and modularisation.

## Project Structure

1. `src/Main.elm` : contains the main application i.e. the calculator app with all the necessary business logic.
2. `src/View.elm` : contains the configurable calculator view, that is used by the Main.elm
3. `src/View/Attributes.elm` contains the attributes for configuring the calculator view defined in View.elm

## Tasks

1. Add more shapes to buttons
   1. Circle
   2. Ellipse
   3. RoundedBox
2. Make the above shapes configurable
   1. RoundedBox with parameters rx and ry
   2. Add parameters to modify fill-color
3. Add the following configurations to viewCalc function. (Hint: modify CalcConfig, View.Attributes)
   1. style string
   2. Number of columns
   3. Add padding to the calculator frame
4. Remove height,width from the CalcConfig and dynamically calculate the height and width based on number of buttons and display-size etc.
5. Add optional configuration (based on list as done in viewCalc) to viewButtons to add following
   1. Parameters for Shapes
   2. Parameters for fill color etc.

More tasks will be added as per the need.

# How to attempt the tasks

    1. Fork this project.
    2. Add me (this username, see Project Information -> Members) as Maintainer.
    3. Create a new branch.
    4. Start making changes on the new branch.
    5. When satisfied with the work, create a PR on gitlab and add me as reviewer in the PR.
    6. Merge changes after review.

_Notes_:

1. All tasks need you to change the view functions, please do not modify the other functions in Main.elm
2. Please use SVG attributes, and not CSS(inline-attributes). I have used them to give border to the calculator, but its not recommended, you can observe the reason by zooming in(Ctrl + in chrome) and zooming out.
