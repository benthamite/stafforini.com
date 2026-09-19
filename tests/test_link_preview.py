"""Run the real preview script against a small DOM/event proxy in Node.

These tests cover content and interaction state, not browser layout or actual
pointer hit-testing. They deliberately need no downloaded DOM dependency.
"""

import json
from pathlib import Path
import shutil
import subprocess

import pytest


REPO_ROOT = Path(__file__).resolve().parents[1]

HARNESS = r"""
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');

class Element {
  constructor(tag = 'div', className = '', text = '') {
    this.nodeType = 1;
    this.tagName = tag.toUpperCase();
    this.className = className;
    this.children = [];
    this.attrs = {};
    this.style = {};
    this.listeners = {};
    this._text = text;
    this.classList = {
      add: name => { this.className = [...new Set([...this.className.split(' '), name])].join(' ').trim(); },
      remove: name => { this.className = this.className.split(' ').filter(x => x !== name).join(' '); },
      contains: name => this.className.split(' ').includes(name),
    };
  }
  get textContent() { return this._text + this.children.map(x => x.textContent).join(''); }
  set textContent(text) { this._text = String(text); this.children = []; }
  set innerHTML(_) { throw new Error('Preview content must not be inserted as HTML'); }
  get childNodes() { return this.children; }
  get firstChild() { return this.children[0] || null; }
  get firstElementChild() { return this.children[0] || null; }
  get id() { return this.attrs.id || ''; }
  set id(value) { this.attrs.id = value; }
  appendChild(child) { child.parentElement = this; child.parentNode = this; this.children.push(child); return child; }
  append(...children) { children.forEach(x => this.appendChild(x)); }
  replaceChildren(...children) { this._text = ''; this.children = []; this.append(...children); }
  removeChild(child) { this.children.splice(this.children.indexOf(child), 1); }
  remove() { this.parentElement.removeChild(this); }
  replaceWith(replacement) {
    const parent = this.parentElement;
    const index = parent.children.indexOf(this);
    parent.children[index] = replacement;
    replacement.parentElement = parent;
    replacement.parentNode = parent;
  }
  cloneNode(deep) {
    const copy = new Element(this.tagName, this.className, this._text);
    copy.attrs = {...this.attrs};
    if (deep) this.children.forEach(child => copy.appendChild(child.cloneNode(true)));
    return copy;
  }
  setAttribute(name, value) { this.attrs[name] = String(value); }
  getAttribute(name) { return this.attrs[name] ?? null; }
  removeAttribute(name) { delete this.attrs[name]; }
  hasAttribute(name) { return name in this.attrs; }
  matches(selector) {
    return selector.split(',').some(part => {
      part = part.trim();
      if (part.startsWith('.')) return this.classList.contains(part.slice(1));
      if (part.startsWith('#')) return this.id === part.slice(1);
      const match = part.match(/^([\w-]+)(?:\[([\w-]+)(?:([\^]?)="([^"]*)")?\])?$/);
      if (!match || this.tagName !== match[1].toUpperCase()) return false;
      if (!match[2]) return true;
      const value = this.getAttribute(match[2]);
      if (value === null) return false;
      return match[4] === undefined || (match[3] === '^' ? value.startsWith(match[4]) : value === match[4]);
    });
  }
  closest(selector) {
    return this.matches(selector) ? this : this.parentElement?.closest(selector) || null;
  }
  contains(other) { return other === this || this.children.some(x => x.contains(other)); }
  querySelectorAll(selector) {
    if (selector.includes(',')) return selector.split(',').flatMap(part => this.querySelectorAll(part.trim()));
    const parts = selector.trim().split(/\s+/);
    const matches = [];
    const walk = node => {
      for (const child of node.children) {
        if (child.matches(parts[parts.length - 1])) {
          let ancestor = child.parentElement;
          let index = parts.length - 2;
          while (ancestor && index >= 0) {
            if (ancestor.matches(parts[index])) index--;
            ancestor = ancestor.parentElement;
          }
          if (index < 0) matches.push(child);
        }
        walk(child);
      }
    };
    walk(this);
    return matches;
  }
  querySelector(selector) { return this.querySelectorAll(selector)[0] || null; }
  addEventListener(type, callback) { (this.listeners[type] ||= []).push(callback); }
  getBoundingClientRect() { return {left: 30, right: 430, top: 20, bottom: 45, width: 400, height: 25}; }
}
const document = new Element('document');
document.body = document.appendChild(new Element('body'));
document.baseURI = 'https://stafforini.com/notes/source/';
document.createElement = tag => new Element(tag);
document.createTextNode = text => {
  const node = new Element('#text', '', text);
  node.nodeType = 3;
  return node;
};
document.activeElement = document.body;
const location = new URL(document.baseURI);
const window = {location, scrollX: 0, scrollY: 0, innerWidth: 1200, innerHeight: 900, addEventListener() {}};
let now = 0;
let nextTimer = 0;
const timers = new Map();
const fetches = [];
const pending = [];
function fetch(url) {
  fetches.push(String(url));
  return new Promise((resolve, reject) => pending.push({resolve, reject}));
}
async function flush() { for (let i = 0; i < 12; i++) await Promise.resolve(); }
async function tick(ms = 150) {
  now += ms;
  for (const [id, timer] of [...timers]) {
    if (timer.at <= now) { timers.delete(id); timer.callback(); }
  }
  await flush();
}
function fire(type, target, relatedTarget = null, key = undefined) {
  const event = {type, target, relatedTarget, key};
  if (type === 'focusin') document.activeElement = target;
  if (type === 'focusout') document.activeElement = relatedTarget || document.body;
  for (const callback of document.listeners[type] || []) callback(event);
  if (target !== document) for (const callback of target.listeners[type] || []) callback(event);
}
function link(href) {
  const el = document.body.appendChild(new Element('a', '', 'Linked work'));
  el.setAttribute('href', href);
  return el;
}
const abstract = 'An abstract with <img src=x onerror=alert(1)> as literal text. ' + 'A substantial discussion of personal identity. '.repeat(12);
function work(withAbstract = true) {
  const doc = new Element('document');
  const header = doc.appendChild(new Element('div', 'work-header'));
  header.appendChild(new Element('h1', '', 'Reasons and persons'));
  header.appendChild(new Element('p', 'work-author', 'Derek Parfit'));
  header.appendChild(new Element('p', 'work-details', 'Oxford, 1984'));
  if (withAbstract) {
    const body = doc.appendChild(new Element('div', 'work-body'));
    body.appendChild(new Element('h2', '', 'Abstract'));
    body.appendChild(new Element('p', '', abstract));
  }
  doc.appendChild(new Element('section', 'backlinks', 'Unrelated citing notes'));
  return doc;
}
const noteText = 'N'.repeat(260);
function note() {
  const doc = new Element('document');
  const body = doc.appendChild(new Element('div', 'note-body'));
  body.appendChild(new Element('p', '', noteText));
  body.appendChild(new Element('p', '', 'Second paragraph should be omitted'));
  return doc;
}
function multilineWork() {
  const doc = work(false);
  const body = doc.appendChild(new Element('div', 'work-body'));
  body.appendChild(new Element('h2', '', 'Abstract'));
  const first = body.appendChild(new Element('p', '', 'First line.'));
  first.appendChild(new Element('br'));
  first.appendChild(new Element('span', '', 'Second line.'));
  body.appendChild(new Element('p', '', 'Next paragraph.'));
  return doc;
}
const fixtures = {work: () => work(), bare: () => work(false), note, multiline: multilineWork};
class DOMParser { parseFromString(key) { return fixtures[key](); } }
async function respond(key = 'work', index = 0) {
  pending[index].resolve({ok: true, text: () => Promise.resolve(key)});
  await flush();
}
function popup() { return document.querySelector('.link-preview'); }
function visible() { return !!popup()?.classList.contains('visible'); }
vm.runInNewContext(fs.readFileSync(process.argv[1], 'utf8'), {
  document, window, location, URL, Node: {ELEMENT_NODE: 1}, DOMParser,
  fetch, Map, console,
  setTimeout(callback, delay) { const id = ++nextTimer; timers.set(id, {callback, at: now + delay}); return id; },
  clearTimeout(id) { timers.delete(id); },
});
"""


def run_scenario(source):
    node = shutil.which("node")
    if not node:
        pytest.skip("Node is required for link preview behavioral tests")
    result = subprocess.run(
        [node, "-e", HARNESS + "\n(async () => {\n" + source +
         "\n})().catch(error => { console.error(error); process.exitCode = 1; });",
         str(REPO_ROOT / "assets/js/link-preview.js")],
        capture_output=True, text=True, timeout=10,
    )
    assert result.returncode == 0, result.stdout + result.stderr


def test_work_preview_contains_metadata_and_full_safe_abstract():
    run_scenario("""
      fire('pointerover', link('/works/parfit1984reasons/'));
      await tick(); await respond();
      assert(visible());
      const text = popup().textContent;
      for (const expected of ['Reasons and persons', 'Derek Parfit', 'Oxford, 1984', abstract.trim()]) {
        assert(text.includes(expected), `Missing preview content: ${expected}`);
      }
      assert(!text.includes('Abstract'));
      assert(!text.includes('Unrelated citing notes'));
      assert.equal(popup().querySelector('img'), null);
    """)


def test_work_without_abstract_still_shows_bibliographic_details():
    run_scenario("""
      fire('pointerover', link('/works/parfit1984reasons/'));
      await tick(); await respond('bare');
      assert(visible());
      assert(popup().textContent.includes('Derek Parfit'));
      assert(popup().textContent.includes('Oxford, 1984'));
    """)


@pytest.mark.parametrize("href", [
    "/notes/example/", "/about/", "/contact/",
    "https://stafforini.com/notes/example/", "../example/",
])
def test_existing_note_previews_remain_short(href):
    run_scenario(f"""
      fire('pointerover', link({json.dumps(href)}));
      await tick(); await respond('note');
      assert(visible());
      assert.equal(popup().textContent, 'N'.repeat(200) + '…');
    """)


@pytest.mark.parametrize("href", [
    "https://elsewhere.example/works/parfit1984reasons/",
    "https://elsewhere.example/notes/example/", "/tags/ethics/", "#section",
])
def test_unrelated_links_do_not_fetch(href):
    run_scenario(f"""
      fire('pointerover', link({json.dumps(href)}));
      await tick();
      assert.equal(fetches.length, 0);
      assert(!visible());
    """)


def test_absolute_same_origin_work_link_and_cache():
    run_scenario("""
      const target = link('https://stafforini.com/works/parfit1984reasons/');
      fire('pointerover', target); await tick(); await respond();
      assert(visible());
      fire('pointerout', target); await tick();
      assert(!visible());
      fire('pointerover', target); await tick();
      assert(visible());
      assert.equal(fetches.length, 1);
    """)


def test_popup_can_be_entered_across_gap_and_left():
    run_scenario("""
      const target = link('/works/parfit1984reasons/');
      fire('pointerover', target); await tick(); await respond();
      fire('pointerout', target);
      await tick(75);
      assert(visible(), 'Must remain visible while crossing the gap');
      fire('pointerenter', popup(), target);
      await tick(200);
      assert(visible(), 'Must remain available for reading and scrolling');
      fire('pointerleave', popup()); await tick();
      assert(!visible());
    """)


def test_keyboard_focus_and_escape():
    run_scenario("""
      const target = link('/works/parfit1984reasons/');
      fire('focusin', target); await tick(); await respond();
      assert(visible());
      fire('keydown', target, null, 'Escape');
      assert(!visible());
    """)


def test_delayed_fetch_cannot_replace_new_link_preview():
    run_scenario("""
      const first = link('/works/parfit1984reasons/');
      const second = link('/notes/example/');
      fire('pointerover', first); await tick();
      fire('pointerout', first, second);
      fire('pointerover', second); await tick();
      await respond('note', 1);
      assert.equal(popup().textContent, 'N'.repeat(200) + '…');
      await respond('work', 0);
      assert.equal(popup().textContent, 'N'.repeat(200) + '…');
    """)


def test_failed_fetch_retries_on_next_hover():
    run_scenario("""
      const target = link('/works/parfit1984reasons/');
      fire('pointerover', target); await tick();
      pending[0].reject(new Error('Network unavailable')); await flush();
      assert(!visible());
      fire('pointerout', target); await tick();
      fire('pointerover', target); await tick();
      assert.equal(fetches.length, 2);
      await respond('work', 1);
      assert(visible());
    """)


def test_reentering_link_before_debounce_still_shows_preview():
    run_scenario("""
      const target = link('/works/parfit1984reasons/');
      fire('pointerover', target); await tick(50);
      fire('pointerout', target); await tick(50);
      fire('pointerover', target); await tick(150);
      assert.equal(fetches.length, 1);
      await respond();
      assert(visible());
    """)


def test_unrelated_pointerout_before_any_link_is_safe():
    run_scenario("""
      fire('pointerout', document.body);
      await tick();
      assert.equal(fetches.length, 0);
      assert(!visible());
    """)


def test_abstract_preserves_paragraphs_and_line_breaks():
    run_scenario("""
      fire('pointerover', link('/works/parfit1984reasons/'));
      await tick(); await respond('multiline');
      assert(visible());
      assert.equal(popup().querySelector('.link-preview-abstract').textContent,
        'First line.\\nSecond line.\\n\\nNext paragraph.');
    """)
