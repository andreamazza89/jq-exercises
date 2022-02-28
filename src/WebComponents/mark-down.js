import * as commonmark from "commonmark"

/*
  Uses the commonmark library to render some read-only markdown.

  If the page contains this custom tag with the markdown string set to the
  "content" attribute, the component will parse the string as markdown and fill the
  <mark-down> node with and html representation of it

*/

class Markdown extends HTMLElement {
    connectedCallback() {
        const rawMarkdown = this.getAttribute("content")

        const parser = new commonmark.Parser()
        const writer = new commonmark.HtmlRenderer();

        this.innerHTML = writer.render(parser.parse(rawMarkdown))
    }
}

export function register() {
    if (window.customElements) {
        window.customElements.define("mark-down", Markdown);
    }
}