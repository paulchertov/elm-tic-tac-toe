module Style exposing (style_tag)
import Html exposing (text, node)
import Html.Attributes exposing (class, type')

style_text = """
.test-app, .test-app *{
    box-sizing: border-box;
}

.test-app{
    width: 300px;
    height: 300px;
    display: block;
}
.test-app > .playground{
    display: block;
    width: 100%;
    height: 100%;
}
.test-app > .playground > .row{
    display: block;
    width: 100%;
    height: 100px;
}
.test-app > .playground > .row > .field{
    display: block;
    width: 100px;
    height: 100px;
    float: left;
    font-size: 45px;
    border: 1px solid #000;
}
.test-app > .playground > .row > .field.win{
    color: red;
}
"""
style_tag =
    node "style" [type' "text/css"] [text style_text]
