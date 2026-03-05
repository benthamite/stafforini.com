(function () {
  var list = document.getElementById('notes-list');
  if (!list) return;

  var STORAGE_KEY = 'notes-sort';
  var buttons = document.querySelectorAll('.sort-btn');

  function sortList(sortBy) {
    var items = Array.from(list.children);

    items.sort(function (a, b) {
      if (sortBy === 'date') {
        var dateA = a.getAttribute('data-date') || '';
        var dateB = b.getAttribute('data-date') || '';
        return dateA < dateB ? 1 : dateA > dateB ? -1 : 0;
      }
      var titleA = a.getAttribute('data-title') || '';
      var titleB = b.getAttribute('data-title') || '';
      return titleA.localeCompare(titleB);
    });

    items.forEach(function (item) { list.appendChild(item); });
  }

  function activate(sortBy) {
    buttons.forEach(function (b) {
      b.classList.toggle('active', b.getAttribute('data-sort') === sortBy);
    });
    sortList(sortBy);
    try { localStorage.setItem(STORAGE_KEY, sortBy); } catch (e) {}
  }

  buttons.forEach(function (btn) {
    btn.addEventListener('click', function () {
      activate(btn.getAttribute('data-sort'));
    });
  });

  // Apply saved preference on load
  var saved = null;
  try { saved = localStorage.getItem(STORAGE_KEY); } catch (e) {}
  if (saved && saved !== 'alpha') {
    activate(saved);
  }
})();
