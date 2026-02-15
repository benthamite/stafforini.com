(function () {
  var list = document.getElementById('notes-list');
  if (!list) return;

  var buttons = document.querySelectorAll('.sort-btn');

  buttons.forEach(function (btn) {
    btn.addEventListener('click', function () {
      buttons.forEach(function (b) { b.classList.remove('active'); });
      btn.classList.add('active');

      var items = Array.from(list.children);
      var sortBy = btn.getAttribute('data-sort');

      items.sort(function (a, b) {
        if (sortBy === 'date') {
          return b.getAttribute('data-date').localeCompare(a.getAttribute('data-date'));
        }
        return a.getAttribute('data-title').localeCompare(b.getAttribute('data-title'));
      });

      items.forEach(function (item) { list.appendChild(item); });
    });
  });
})();
