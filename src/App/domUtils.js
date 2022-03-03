/*
  Scrolls the jq-input box to the top.

  This is far from ideal, as have an implicit dependency on the
  caller to set the id on the node we want to scroll.

  The node id could be parametrised, but we can look into that if
  we ever need scrolling anywhere else.
*/

exports.scrollJqInputIntoView = function scrollJqInputIntoView() {
    if (document) {
        document
            .getElementById("jq-input")
            .scrollIntoView({behavior: "smooth", block: "center"})
    }
};
