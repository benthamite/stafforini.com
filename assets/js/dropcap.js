// Add .dropcap class to the first prose <p> in .has-dropcap, skipping
// leading blockquotes and their attribution paragraphs.
(function () {
  var body = document.querySelector('.has-dropcap');
  if (!body) return;
  var children = body.children;
  for (var i = 0; i < children.length; i++) {
    var el = children[i];
    if (el.tagName === 'BLOCKQUOTE') continue;
    if (el.tagName === 'P' && i > 0 && children[i - 1].tagName === 'BLOCKQUOTE') continue;
    if (el.tagName === 'P') {
      el.classList.add('dropcap');
      return;
    }
    return;
  }
})();
