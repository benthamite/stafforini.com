// Add .dropcap class to the first prose <p> in .has-dropcap, skipping
// leading blockquotes, their attribution paragraphs, and fully italicized
// paragraphs (editorial notes/asides are not the opening of the prose).
(function () {
  var body = document.querySelector('.has-dropcap');
  if (!body) return;
  var children = body.children;
  for (var i = 0; i < children.length; i++) {
    var el = children[i];
    if (el.tagName === 'BLOCKQUOTE') continue;
    if (el.tagName === 'P' && i > 0 && children[i - 1].tagName === 'BLOCKQUOTE') continue;
    if (el.tagName === 'P' && isItalicAside(el)) continue;
    if (el.tagName === 'P') {
      el.classList.add('dropcap');
      return;
    }
    return;
  }

  function isItalicAside(p) {
    var first = p.firstElementChild;
    if (!first) return false;
    if (first.tagName !== 'EM' && first.tagName !== 'I') return false;
    return first.textContent.trim() === p.textContent.trim();
  }
})();
