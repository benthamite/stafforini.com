"""Execute search pagination races against the real JavaScript module."""

from pathlib import Path
import subprocess


ROOT = Path(__file__).resolve().parents[1]


def test_pending_pagination_cannot_append_to_a_replacement_query():
    subprocess.run(["node", "-e", r"""
const assert = require('node:assert/strict');
const vm = require('node:vm');
const fs = require('node:fs');
const context = {window: {_searchUtils: {renderResult: r => `<li>${r.label}</li>`}}};
vm.createContext(context);
vm.runInContext(fs.readFileSync('assets/js/search-core.js', 'utf8'), context);
(async () => {
  let resolveOld;
  let requests = 0;
  const pendingData = new Promise(resolve => { resolveOld = resolve; });
  context.storeSectionResults('test', 'notes', 'old', [{data: () => {
    requests++;
    return pendingData;
  }}], 0, 25);
  const button = {disabled: false, remove() {}};
  const list = {html: '', insertAdjacentHTML(_, html) { this.html += html; }};
  const options = {button, resultsEl: {querySelector() {return {querySelector() {return list;}};}}};
  const oldPage = context.showMoreResults('test', 'notes', options);
  const repeatedPage = context.showMoreResults('test', 'notes', options);
  let generation = 0;
  await context.runSearch('new', {search: async () => ({results: [
    {data: async () => ({label: 'new query result'})}
  ]})}, {
    instanceId: 'test', maxResultsPerSection: 25, filterSection: 'notes',
    onResults: html => {list.html = html;},
    incrementGeneration: () => ++generation, getGeneration: () => generation
  });
  const newHtml = list.html;
  assert(newHtml.includes('new query result'));
  resolveOld({label: 'old query result'});
  await oldPage;
  await repeatedPage;
  assert.equal(list.html, newHtml, 'Old-query results must not alter the replacement query');
  assert.equal(requests, 1, 'Repeated keyboard activation must not duplicate the pending batch');
  context.storeSectionResults('test', 'notes', 'new', [{data: async () => ({label: 'more new results'})}], 0, 25);
  button.disabled = false;
  await context.showMoreResults('test', 'notes', options);
  assert(list.html.includes('more new results'), 'Current-query pagination must still work');
})().catch(err => {console.error(err); process.exitCode = 1;});
"""], cwd=ROOT, check=True, capture_output=True, text=True)
