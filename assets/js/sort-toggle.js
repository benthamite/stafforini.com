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
          var dateA = a.getAttribute('data-date') || '';
          var dateB = b.getAttribute('data-date') || '';
          return dateB.localeCompare(dateA);
        }
        var titleA = a.getAttribute('data-title') || '';
        var titleB = b.getAttribute('data-title') || '';
        return titleA.localeCompare(titleB);
      });

      items.forEach(function (item) { list.appendChild(item); });
    });
  });
})();
