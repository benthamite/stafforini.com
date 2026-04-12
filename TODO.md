# TODO

## Auto-populate work tags from external keyword sources

~21,750 bib entries across 4 files; only 632 (2.9%) currently have keywords. The Hugo templates already render tags on work pages — the gap is that `generate-work-pages.py` doesn't extract the `keywords` field and no script populates it from external sources.

### Pipeline

1. **Wire existing keywords** — Add `"keywords"` to `extra_fields` in `generate-work-pages.py` so the 632 entries that already have keywords get tags immediately.
2. **OpenAlex bulk fetch (primary source)** — Query OpenAlex API by DOI for the ~7,300 DOI-bearing entries. Extract subfields (252-category taxonomy), author keywords, and selectively the primary topic. Write back to bib `keywords` fields. Free, 10 req/sec, ~12 min total.
3. **Books via Zotra/LoC (ISBN entries without DOI)** — Fetch via Zotra's `/search` endpoint for ISBN-bearing entries not covered by Phase 2. LCSH subject headings come through automatically. Callable via `zotra-get-entry` or the CLI (`node ~/source/zotra-server/bin/index.js search <isbn>`).
4. **OpenAlex title search (no DOI, no ISBN)** — Fall back to OpenAlex title+author search for remaining entries. Lower confidence but decent coverage.
5. **Normalization** — Merge synonyms, standardize casing, align with existing note/quote tag vocabulary. Most human judgment needed here.

### Source preference (when multiple sources overlap)

1. **OpenAlex subfields** (252 categories) — universal, consistent, browsable; backbone of the taxonomy
2. **Author keywords from OpenAlex** — specific-topic supplement (needs normalization)
3. **LCSH** (via Zotra/LoC) — higher quality than OpenAlex for books
4. **MeSH** (via PubMed) — gold standard for biomedical works
5. **OpenAlex topics** (~4,500) — only when label is human-readable and confidence > 0.5

### Key facts

- Zotero/Zotra DOI lookups go through CrossRef, which returns **zero keywords** — this is why we need direct OpenAlex integration
- Zotero/Zotra ISBN lookups work well — LoC provides LCSH subject headings
- Expected coverage: 70–85% from APIs; LLM fallback could push to 90%+
- Entries with DOI: 7,320 (33.7%); with ISBN: 5,250 (24.1%)

## Export unexported PDF highlights

172 PDFs have highlights not yet exported to their org notes. Run `python3 scripts/check-unexported-highlights.py` to regenerate the list. Use `--min-gap 3` to filter out likely false positives from cross-page highlight splitting.

Top priority (largest gaps):

- [ ] McGonigal2012WillpowerInstinct (207 unexported)
- [ ] Ariely2008PredictablyIrrationalHidden (156)
- [ ] Cooney2013VeganomicsSurprisingScience (143)
- [ ] Baumeister2010ThereAnythingGood (99)
- [ ] Wiseman2007QuirkologyHowWe (91)
- [ ] Boswell1791LifeSamuelJohnson (69)
- [ ] Levine2003PowerPersuasionHow (64)
- [ ] Shirer1941BerlinDiaryJournal (63)
- [ ] Elster2010ExplainingSocialBehavior (61)
- [ ] Baumeister2010AdvancedSocialPsychology (52)
- [ ] Cadicamo1972DesconocidoJuanCarlos (49)
- [ ] Phillips2012HackingHappy (46)
- [ ] Chalmers2002PhilosophyMindClassical (45)
- [ ] Budiansky2021JourneyEdgeReason (41)
- [ ] Copp2019MoralityNaturalWorld (39)
- [ ] Broome2005ShouldWeValue (38)
- [ ] Elster2016StatesThatAre (37)
- [ ] Suber2012OpenAccess (37)
- [ ] Hunt2010MarxGeneralRevolutionary (35)
- [ ] Moen2013UnityCommensurabilityPleasures (35)
- [ ] Rosenberg2003NonviolentCommunicationLanguage (33)
- [ ] Caro2003MasterSenate (32)
- [ ] Winter2013MotivationHacker (32)
- [ ] Cowen2007CaringDistantFuture (29)
- [ ] Hargittai2006MartiansScienceFive (28)
- [ ] Cooney2011ChangeHeartWhat (26)
- [ ] BioyCasares2006Borges (24)
- [ ] Grunbaum2009WhyThereWorld (23)
- [ ] Loughnan2010RoleMeatConsumption (23)
- [ ] Gunaratana1991MindfulnessPlainEnglish (21)
- [ ] Miller2012SingularityRisingSurviving (20)
- [ ] Thompson2022FilmHistoryIntroduction (19)
- [ ] Carlson2001PresumptionNothingness (18)
- [ ] Broad1925MindItsPlace (17)
- [ ] Burns2009GoddessMarketAyn (17)
- [ ] Colvin2008TalentOverratedWhat (17)
- [ ] Hamming1986YouAndYour (17)
- [ ] Knize2008GrandObsessionPiano (17)
- [ ] Broome2004WeighingLives (16)
- [ ] Walster1966ImportancePhysicalAttractiveness (16)
- [ ] Chalmers2014UploadingPhilosophicalAnalysis (15)
- [ ] Sapontzis1984Predation (14)
- [ ] Finnveden2022AGILockin (13)
- [ ] Chang2010PriceHappyHens (12)
- [ ] Hofmann1920PianoPlayingWith (12)
- [ ] Keyes1959FlowersAlgernon (12)
- [ ] Sobel2007ImpotenceDemandingnessObjection (12)
- [ ] Styron1990DarknessVisibleMemoir (12)
