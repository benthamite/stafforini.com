// Forward bare-root PDF URLs (e.g. /parfit-2016-conflicting-reasons.pdf) to
// the canonical R2 host. Replaces the `_redirects` rule
// `/:slug.pdf  https://pdf.stafforini.com/:slug.pdf  301!`, which Netlify's
// `_redirects` parser misinterpreted as a wildcard on any single-segment
// path because the dot was absorbed into the placeholder name.
//
// URLPattern (used by Netlify edge function `path` config) follows the web
// standard: `:slug` is `[^/]+`, and the trailing `.pdf` is a literal, so this
// fires only on apex `<slug>.pdf` URLs.

export default async (request: Request) => {
  const url = new URL(request.url);
  return Response.redirect(
    `https://pdf.stafforini.com${url.pathname}`,
    301,
  );
};

export const config = {
  path: "/:slug.pdf",
};
