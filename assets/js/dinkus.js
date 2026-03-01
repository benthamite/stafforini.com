// Randomly assign one of five ornamental dinkus styles to each <hr> element.
// Skips <hr> elements inside .footnotes (those are hidden by CSS).
(function () {
  var count = 5;
  var hrs = document.querySelectorAll('hr:not(.footnotes > hr)');
  for (var i = 0; i < hrs.length; i++) {
    var n = Math.floor(Math.random() * count) + 1;
    hrs[i].classList.add('dinkus-' + n);
  }
})();
