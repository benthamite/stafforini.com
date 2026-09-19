"""Regression checks for sidenote collision and hover behavior."""

from pathlib import Path
import re
import subprocess


REPO_ROOT = Path(__file__).resolve().parents[1]
SIDENOTES_CSS = REPO_ROOT / "assets" / "css" / "_sidenotes.css"
CONTENT_CSS = REPO_ROOT / "assets" / "css" / "_content.css"
SIDENOTES_JS = REPO_ROOT / "assets" / "js" / "sidenotes.js"


def _css_block(css: str, selector: str) -> str:
    pattern = re.compile(rf"{re.escape(selector)}\s*\{{(?P<body>.*?)\}}", re.S)
    match = pattern.search(css)
    assert match is not None, f"Missing CSS block for {selector}"
    return match.group("body")


def _z_index(css: str, selector: str) -> int:
    body = _css_block(css, selector)
    match = re.search(r"\bz-index\s*:\s*(-?\d+)\s*;", body)
    assert match is not None, f"Missing z-index for {selector}"
    return int(match.group(1))


def test_sidenote_column_paints_above_code_masks():
    """Hover masks for code blocks must not cover neighboring sidenotes."""
    css = SIDENOTES_CSS.read_text()

    assert _z_index(css, ".sidenote-column") > _z_index(css, ".sidenote-code-mask")


def test_closed_details_code_is_not_a_sidenote_obstacle():
    """Hidden code geometry must not truncate neighboring sidenotes."""
    js = SIDENOTES_JS.read_text()
    skip = js.index("if (pre.closest('details:not([open])')) return;")
    measure = js.index("var rect = pre.getBoundingClientRect();", skip)

    assert skip < measure


def test_footnote_reference_does_not_expand_body_line_box():
    """Superscript references must preserve the body-text baseline rhythm."""
    css = CONTENT_CSS.read_text()
    body = _css_block(css, 'sup[id^="fnref:"]')

    assert re.search(r"\bline-height\s*:\s*0\s*;", body)


def test_keyboard_reveals_clipped_sidenotes_and_preserves_mobile_targets():
    """Exercise real handlers and layout code; browser focus navigation is separate."""
    subprocess.run(["node", "-e", r"""
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
class Element {
  constructor(tag, className = '', id = '') {
    this.tagName = tag.toUpperCase(); this.className = className; this.id = id;
    this.children = []; this.style = {}; this.attrs = {}; this.listeners = {};
    this.scrollHeight = 200;
    this.classList = {
      add: name => {if (!this.className.split(' ').includes(name)) this.className += ' ' + name;},
      remove: name => {this.className = this.className.split(' ').filter(x => x !== name).join(' ');},
      contains: name => this.className.split(' ').includes(name)
    };
  }
  appendChild(el) {if (el.parent) el.remove(); this.children.push(el); el.parent = this; return el;}
  get firstChild() {return this.children[0];}
  remove() {this.parent.children.splice(this.parent.children.indexOf(this), 1);}
  setAttribute(k, v) {this.attrs[k] = v;}
  getAttribute(k) {return this.attrs[k] ?? null;}
  removeAttribute(k) {if (k === 'id') this.id = ''; else delete this.attrs[k];}
  cloneNode() {const el = new Element(this.tagName, this.className, this.id); this.children.forEach(x => el.appendChild(x.cloneNode())); return el;}
  matches(s) {
    if (s[0] === '.') return this.classList.contains(s.slice(1));
    if (s === 'li[id^="fn:"]') return this.tagName === 'LI' && this.id.startsWith('fn:');
    return this.tagName.toLowerCase() === s;
  }
  querySelectorAll(selector) {
    const result = [];
    this.children.forEach(el => {
      if (selector.split(',').some(s => el.matches(s.trim()))) result.push(el);
      result.push(...el.querySelectorAll(selector));
    });
    return result;
  }
  querySelector(s) {return this.querySelectorAll(s)[0] || null;}
  closest(s) {return this.matches(s) ? this : this.parent?.closest(s) || null;}
  contains(el) {return el === this || this.children.some(x => x.contains(el));}
  addEventListener(event, fn) {(this.listeners[event] ||= []).push(fn);}
  getBoundingClientRect() {return {top: this.refTop || 0, left: 0, right: 500, width: 500, height: 200};}
}
const document = new Element('document');
document.body = document.appendChild(new Element('body'));
document.activeElement = document.body;
document.createElement = tag => new Element(tag);
document.getElementById = id => {
  function find(el) {return el.id === id ? el : el.children.map(find).find(Boolean);}
  return find(document);
};
const container = document.body.appendChild(new Element('div', 'content-with-sidenotes'));
const body = container.appendChild(new Element('div', 'note-body'));
const column = container.appendChild(new Element('aside', 'sidenote-column'));
const footnotes = body.appendChild(new Element('div', 'footnotes'));
const refs = [1, 2].map(num => {
  const ref = body.appendChild(new Element('sup', '', 'fnref:' + num));
  ref.refTop = 30 * num;
  const link = ref.appendChild(new Element('a'));
  link.setAttribute('href', '#fn:' + num);
  footnotes.appendChild(new Element('li', '', 'fn:' + num)).appendChild(new Element('p'));
  return link;
});
let activate, deactivate;
const window = {addEventListener() {}, createBreakpointToggle(_, on, off) {
  activate = on; deactivate = off; on(); return {isActive: () => true};
}};
vm.runInNewContext(fs.readFileSync('assets/js/sidenotes.js', 'utf8'), {
  document, window, getComputedStyle: () => ({lineHeight: '20px', fontSize: '16px'})
});
const first = column.children[0];
const fire = (el, event, relatedTarget = null) => (el.listeners[event] || []).forEach(fn => fn({relatedTarget}));
assert.equal(refs[0].getAttribute('href'), '#' + first.id);
assert.equal(document.getElementById(first.id), first);
assert.equal(first.getAttribute('tabindex'), '-1');
assert.equal(first.style.height, '60px', 'Dense fixture must initially clip the first long note');
document.activeElement = refs[0]; fire(refs[0], 'focusin');
assert.equal(first.style.height, '200px', 'Keyboard focus must reveal the complete note');
fire(refs[0], 'mouseenter'); fire(refs[0], 'mouseleave');
assert.equal(first.style.height, '200px', 'Pointer departure must not collapse keyboard focus');
document.activeElement = first;
fire(refs[0], 'focusout', first); fire(first, 'focusin');
assert.equal(first.style.height, '200px', 'Following the sidenote anchor must preserve expansion');
document.activeElement = document.body; fire(first, 'focusout', document.body);
assert.equal(first.style.height, '60px');
deactivate();
assert.equal(refs[0].getAttribute('href'), '#fn:1', 'Narrow screens retain their original footnote targets');
assert(!document.body.classList.contains('has-sidenotes'));
activate();
assert.equal(refs[0].getAttribute('href'), '#' + first.id);
"""], cwd=REPO_ROOT, check=True, capture_output=True, text=True)
