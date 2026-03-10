// Assign one of five ornamental dinkus styles to each <hr> element.
// Uses a hash of the page path + hr index for deterministic assignment.
(function () {
  var count = 5;
  var path = window.location.pathname;
  var hrs = document.querySelectorAll('hr:not(.footnotes > hr)');
  for (var i = 0; i < hrs.length; i++) {
    var str = path + '#' + i;
    // djb2 string hash — produces a deterministic 32-bit integer from a string.
    var hash = 0;
    for (var j = 0; j < str.length; j++) {
      hash = ((hash << 5) - hash) + str.charCodeAt(j);
      hash |= 0; // convert to 32-bit signed integer
    }
    var n = (Math.abs(hash) % count) + 1;
    hrs[i].classList.add('dinkus-' + n);
  }
})();
