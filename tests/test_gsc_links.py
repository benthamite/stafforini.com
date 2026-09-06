"""Offline contracts for the paired GSC skill helper. No Gmail/HTTP calls."""

import base64
import contextlib
from email.message import Message
import importlib.util
import io
import json
from pathlib import Path
import runpy
import subprocess
import sys
import unittest
from unittest.mock import Mock, patch
import urllib.error


ROOT = Path(__file__).resolve().parents[1]
HELPER = ROOT / ".claude/skills/gsc-indexing-triage/scripts/extract-gsc-links.py"
PAIR = ROOT / ".codex/skills/gsc-indexing-triage/scripts/extract-gsc-links.py"
SPEC = importlib.util.spec_from_file_location("gsc_links", HELPER)
gsc = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(gsc)
SHORT = "https://c.gle/fixture"
DIRECT = "https://search.google.com/u/1/search-console/index/drilldown?resource_id=sc-domain%3Astafforini.com"


def part(body, mime="text/html", charset="utf-8"):
    return {
        "mimeType": mime,
        "headers": [{"name": "Content-Type", "value": f"{mime}; charset={charset}"}],
        "body": {"data": base64.urlsafe_b64encode(body.encode(charset)).decode().rstrip("=")},
    }


def message(body="", message_id="abc"):
    payload = part(body)
    payload["headers"].append({"name": "Subject", "value": "Page indexing for site stafforini.com"})
    return {"id": message_id, "threadId": "def", "payload": payload}


def button(url=SHORT, label="View issue details"):
    return f'<a href="{url}">{label}</a>'


def http_error(code=302, targets=(DIRECT,)):
    headers = Message()
    for value in targets:
        headers["Location"] = value
    return urllib.error.HTTPError(SHORT, code, "PRIVATE SERVER TEXT", headers, io.BytesIO(b"PRIVATE BODY"))


class GscLinksTests(unittest.TestCase):
    def setUp(self):
        # Any unmocked side-effect boundary is a test failure, not a live probe.
        self.addCleanup(patch.stopall)
        patch("socket.socket", side_effect=AssertionError("network forbidden")).start()
        patch.object(subprocess, "run", side_effect=AssertionError("Gmail forbidden")).start()

    def raw(self, value, message_id="abc"):
        def emit(*args, **kwargs):
            kwargs["stdout"].write(json.dumps(value).encode())
        with patch.object(subprocess, "run", side_effect=emit):
            return gsc.raw_message(message_id, "personal")

    def cli(self, responses, args):
        def emit(command, **kwargs):
            value = responses[command[3]]
            if isinstance(value, Exception):
                raise value
            kwargs["stdout"].write(json.dumps(value).encode())
        output, errors = io.StringIO(), io.StringIO()
        with patch.object(subprocess, "run", side_effect=emit), \
                patch.object(sys, "argv", [str(HELPER), *args]), \
                contextlib.redirect_stdout(output), contextlib.redirect_stderr(errors):
            with self.assertRaises(SystemExit) as raised:
                runpy.run_path(str(HELPER), run_name="__main__")
        return raised.exception.code, output.getvalue(), errors.getvalue()

    def test_pair_is_byte_identical(self):
        self.assertEqual(HELPER.read_bytes(), PAIR.read_bytes())

    def test_anchor_boundary_does_not_capture_unrelated_link(self):
        html = '<a href="https://attacker.example/track">unrelated</a>' + button()
        self.assertEqual(gsc.view_issue_links(message(html)), [SHORT])

    def test_nested_label_case_whitespace_and_entities(self):
        html = '<A HREF = "https://c.gle/fixture?a=1&amp;b=2"> VIEW <b>issue</b>&nbsp;details </A>'
        self.assertEqual(gsc.view_issue_links(message(html)), [SHORT + "?a=1&b=2"])

    def test_entities_decode_once_only(self):
        html = button(SHORT + "?q=a&amp;amp;b")
        self.assertEqual(gsc.view_issue_links(message(html)), [SHORT + "?q=a&amp;b"])

    def test_hidden_or_outside_label_does_not_match(self):
        for html in [
            button(label="<script>View issue details</script>other"),
            button(label="<style>View issue details</style>other"),
            '<a href="https://c.gle/fixture">other</a> View issue details',
            '<a href="https://c.gle/fixture">View issue details',
        ]:
            with self.subTest(html=html):
                self.assertEqual(gsc.view_issue_links(message(html)), [])

    def test_malformed_duplicate_and_missing_href_are_not_actionable(self):
        for html in [
            '<a href="https://c.gle/a" href="https://c.gle/b">View issue details</a>',
            '<a>View issue details</a>',
        ]:
            with self.subTest(html=html), patch.object(gsc, "raw_message", return_value=message(html)):
                self.assertEqual(gsc.message_rows("abc", "personal")[0]["status"], "rejected_link")

    def test_nested_anchor_abandons_first(self):
        html = '<a href="https://attacker.example/a">old ' + button()
        self.assertEqual(gsc.view_issue_links(message(html)), [SHORT])

    def test_duplicate_links_across_mime_parts_are_deduplicated(self):
        msg = message()
        msg["payload"] = {"mimeType": "multipart/alternative", "parts": [part(button()), part(button())]}
        self.assertEqual(gsc.view_issue_links(msg), [SHORT])

    def test_link_limit_is_explicit(self):
        with patch.object(gsc, "MAX_LINKS", 1):
            with self.assertRaisesRegex(gsc.ExtractionError, "link_limit_exceeded"):
                gsc.view_issue_links(message(button() + button(SHORT + "2")))

    def test_base64_accepts_unpadded_unicode_and_declared_charset(self):
        self.assertEqual(gsc.bodies(message("á"), "text/html"), ["á"])
        msg = message()
        msg["payload"] = part("café", charset="iso-8859-1")
        self.assertEqual(gsc.bodies(msg, "text/html"), ["café"])

    def test_base64_rejects_corruption_and_invalid_charset(self):
        for data, charset in [("%%%", "utf-8"), ("A", "utf-8"), ("_w", "utf-8"), ("YQ", "not-a-codec")]:
            with self.subTest(data=data, charset=charset):
                with self.assertRaisesRegex(gsc.ExtractionError, "invalid_body_encoding"):
                    gsc.decode_body(data, charset)

    def test_attachment_body_is_incomplete_not_no_link(self):
        msg = message()
        msg["payload"]["body"] = {"attachmentId": "owned-fixture", "size": 123}
        with patch.object(gsc, "raw_message", return_value=msg):
            row = gsc.message_rows("abc", "personal")[0]
        self.assertEqual((row["status"], row["error"]), ("message_error", "body_requires_attachment"))

    def test_raw_success_exact_command_timeout_and_handle_cleanup(self):
        files = []
        def emit(command, **kwargs):
            self.assertEqual(command, [sys.executable, str(gsc.GMAIL), "raw", "abc", "--account", "epoch"])
            self.assertEqual(kwargs["timeout"], 30)
            self.assertTrue(kwargs["check"])
            self.assertEqual(kwargs["stderr"], subprocess.DEVNULL)
            files.append(kwargs["stdout"])
            kwargs["stdout"].write(json.dumps(message()).encode())
        with patch.object(subprocess, "run", side_effect=emit):
            self.assertEqual(gsc.raw_message("abc", "epoch")["id"], "abc")
        self.assertTrue(files[0].closed)

    def test_raw_temporary_file_ignores_drive_tmpdir(self):
        original = gsc.tempfile.TemporaryFile
        with patch.object(gsc.tempfile, "TemporaryFile", wraps=original) as temporary:
            self.raw(message())
        temporary.assert_called_once_with(dir="/tmp")

    def test_invalid_id_never_spawns_gmail(self):
        for value in ["", "-x", "../mail", "abc?format=raw", "a\nb", "a" * 257]:
            with self.subTest(value=value):
                with self.assertRaisesRegex(gsc.ExtractionError, "invalid_message_id"):
                    gsc.raw_message(value, "personal")

    def test_raw_shape_and_message_identity_fail_closed(self):
        malformed = [[], None, {}, message(message_id="wrong")]
        for field, value in [("threadId", []), ("threadId", ""), ("payload", [])]:
            msg = message()
            msg[field] = value
            malformed.append(msg)
        for value in malformed:
            with self.subTest(value=value):
                with self.assertRaisesRegex(gsc.ExtractionError, "invalid_message_shape"):
                    self.raw(value)

    def test_invalid_mime_structure_fails_before_http(self):
        bad_parts = [None, {"parts": {}}, {"headers": [None]}, {"headers": [{"name": "a", "value": 1}]},
                     {"body": []}, {"body": {"data": []}}, {"body": {"size": True}},
                     {"body": {"size": -1}}, {"mimeType": []}, {"body": {"attachmentId": []}}]
        for value in bad_parts:
            msg = message(button())
            msg["payload"]["parts"] = [value]
            with self.subTest(value=value):
                with self.assertRaisesRegex(gsc.ExtractionError, "invalid_message_shape"):
                    self.raw(msg)

    def test_mime_depth_and_part_limits(self):
        msg = message()
        msg["payload"]["parts"] = [part("a"), part("b")]
        for name, value in [("MAX_DEPTH", 0), ("MAX_PARTS", 1)]:
            with self.subTest(name=name), patch.object(gsc, name, value):
                with self.assertRaisesRegex(gsc.ExtractionError, "mime_limit_exceeded"):
                    self.raw(msg)

    def test_raw_errors_are_bounded_and_private_output_is_suppressed(self):
        failures = [
            (subprocess.TimeoutExpired("PRIVATE COMMAND", 30, output="PRIVATE"), "gmail_timeout"),
            (subprocess.CalledProcessError(1, "PRIVATE COMMAND", stderr="PRIVATE"), "gmail_failed"),
            (OSError("PRIVATE PATH"), "gmail_failed"),
        ]
        for failure, code in failures:
            with self.subTest(code=code), patch.object(subprocess, "run", side_effect=failure):
                with self.assertRaises(gsc.ExtractionError) as raised:
                    gsc.raw_message("abc", "personal")
                self.assertEqual(str(raised.exception), code)

    def test_raw_bad_json_and_size_limit(self):
        for raw, expected in [(b"{PRIVATE", "invalid_message_json"),
                              (b"\xff", "invalid_message_json"),
                              (b"0123456789", "message_too_large")]:
            def emit(*args, **kwargs):
                kwargs["stdout"].write(raw)
            with self.subTest(raw=raw), patch.object(gsc, "MAX_RAW_BYTES", 9), \
                    patch.object(subprocess, "run", side_effect=emit):
                with self.assertRaisesRegex(gsc.ExtractionError, expected):
                    gsc.raw_message("abc", "personal")

    def test_allowed_direct_variants_do_not_open_network(self):
        for url in [DIRECT, "https://search.google.com/search-console/index/drilldown",
                    "https://SEARCH.GOOGLE.COM:443/u/42/search-console/"]:
            with self.subTest(url=url), patch.object(gsc.urllib.request, "build_opener") as opened:
                self.assertEqual(gsc.resolve_once(url), url)
                opened.assert_not_called()

    def test_unsafe_urls_never_open_network(self):
        urls = [
            "https://attacker.example/x", "http://c.gle/x", "ftp://c.gle/x",
            "https://c.gle.attacker.example/x", "https://attacker.example@c.gle/x",
            "https://@c.gle/x", "https://c.gle:444/x", "https://c.gle:bad/x",
            "https://c.gle./x", "https://c.gle/", "https://c.gle",
            "https://search.google.com/other", "https://search.google.com/search-console-evil/",
            "https://search.google.com/u/evil/search-console/", "//c.gle/x",
            "https://search.google.com/search-console/../../url?q=https://attacker.example",
            "https://search.google.com/search-console/%2e%2e/%2e%2e/url",
            "https://search.google.com/search-console/%2F..%2F../url",
            "https://search.google.com/search-console/%5c..%5curl",
            "https://c.gle/%00secret", "https://c.gle/%ff",
            "\nhttps://c.gle/x", "https://c.gle/x\t", "https://c.gle/a b",
            "https://c.gle\\@attacker.example/x", "https://c.gle/é", "https://[broken",
            "https://127.0.0.1/private", "file:///private/secret", "https://c.gle/" + "x" * 8192,
        ]
        for url in urls:
            with self.subTest(url=url), patch.object(gsc.urllib.request, "build_opener") as opened:
                with self.assertRaisesRegex(gsc.ExtractionError, "unsafe_url"):
                    gsc.resolve_once(url)
                opened.assert_not_called()

    def test_http_errors_are_errors_and_handles_close(self):
        for code in [400, 401, 403, 404, 429, 500, 503]:
            error = http_error(code)
            opener = Mock()
            opener.open.side_effect = error
            with self.subTest(code=code), patch.object(gsc.urllib.request, "build_opener", return_value=opener):
                with self.assertRaisesRegex(gsc.ExtractionError, f"^http_{code}$"):
                    gsc.resolve_once(SHORT)
                self.assertTrue(error.closed)
                self.assertEqual(opener.open.call_count, 1)

    def test_redirect_codes_and_protocol_relative_location_resolve_once(self):
        for code in gsc.REDIRECT_CODES:
            error = http_error(code, (DIRECT.removeprefix("https:"),))
            opener = Mock()
            opener.open.side_effect = error
            with self.subTest(code=code), patch.object(gsc.urllib.request, "build_opener", return_value=opener):
                self.assertEqual(gsc.resolve_once(SHORT), DIRECT)
                self.assertTrue(error.closed)
                self.assertEqual(opener.open.call_count, 1)
                request = opener.open.call_args.args[0]
                self.assertEqual((request.full_url, request.get_method()), (SHORT, "GET"))
                self.assertEqual(opener.open.call_args.kwargs, {"timeout": 20})
                self.assertFalse(request.has_header("Authorization"))
                self.assertFalse(request.has_header("Cookie"))

    def test_invalid_or_multiple_locations_and_chain_are_explicit(self):
        cases = [
            ((), "missing_or_ambiguous_location"),
            ((DIRECT, DIRECT), "missing_or_ambiguous_location"),
            (("/next",), "unsupported_redirect_chain"),
            (("https://c.gle/next",), "unsupported_redirect_chain"),
            (("https://attacker.example/x",), "unsafe_url"),
            (("http://search.google.com/search-console/",), "unsafe_url"),
            (("\n" + DIRECT,), "unsafe_url"),
            (("https://[broken",), "unsafe_url"),
        ]
        for targets, code in cases:
            error = http_error(targets=targets)
            opener = Mock()
            opener.open.side_effect = error
            with self.subTest(targets=targets), patch.object(gsc.urllib.request, "build_opener", return_value=opener):
                with self.assertRaisesRegex(gsc.ExtractionError, code):
                    gsc.resolve_once(SHORT)
                self.assertTrue(error.closed)
                self.assertEqual(opener.open.call_count, 1)

    def test_successful_html_response_is_not_false_resolution_and_closes(self):
        response = io.BytesIO(b"not an issue")
        opener = Mock()
        opener.open.return_value = response
        with patch.object(gsc.urllib.request, "build_opener", return_value=opener):
            with self.assertRaisesRegex(gsc.ExtractionError, "no_redirect"):
                gsc.resolve_once(SHORT)
        self.assertTrue(response.closed)

    def test_network_failure_is_bounded(self):
        for failure in [urllib.error.URLError("PRIVATE"), TimeoutError("PRIVATE"),
                        gsc.http.client.BadStatusLine("PRIVATE")]:
            opener = Mock()
            opener.open.side_effect = failure
            with self.subTest(failure=failure), patch.object(gsc.urllib.request, "build_opener", return_value=opener):
                with self.assertRaisesRegex(gsc.ExtractionError, "^link_request_failed$"):
                    gsc.resolve_once(SHORT)

    def test_redirect_handler_never_follows(self):
        request = gsc.urllib.request.Request(SHORT)
        handler = gsc.NoRedirect()
        for code in gsc.REDIRECT_CODES:
            self.assertIsNone(handler.redirect_request(request, None, code, "", Message(), DIRECT))

    def test_cli_continues_after_message_failure_and_reports_partial(self):
        good = message(button(DIRECT), message_id="good")
        result, output, errors = self.cli(
            {"bad": subprocess.CalledProcessError(1, "PRIVATE"), "good": good},
            ["--json", "bad", "good"],
        )
        rows = json.loads(output)
        self.assertEqual(result, 1)
        self.assertEqual([r["status"] for r in rows], ["message_error", "direct"])
        self.assertEqual(rows[1]["issue_url"], DIRECT)
        self.assertNotIn("PRIVATE", output + errors)
        self.assertEqual(errors, "")

    def test_cli_no_link_is_explicit_incomplete(self):
        code, output, errors = self.cli({"abc": message()}, ["--json", "abc"])
        self.assertEqual(code, 1)
        self.assertEqual(json.loads(output)[0]["status"], "no_link")
        self.assertEqual(errors, "")

    def test_cli_multiple_link_rows_preserve_success_and_rejection(self):
        msg = message(button("https://attacker.example/x") + button(DIRECT))
        code, output, errors = self.cli({"abc": msg}, ["--json", "abc"])
        rows = json.loads(output)
        self.assertEqual(code, 1)
        self.assertEqual([r["status"] for r in rows], ["rejected_link", "direct"])
        self.assertEqual(rows[0]["issue_url"], "")
        self.assertEqual(errors, "")

    def test_cli_json_unicode_and_tsv_line_framing(self):
        msg = message(button(DIRECT))
        subject = "España\tone\r\ntwo\x00end\u2028next\x85line\ud800"
        msg["payload"]["headers"][-1]["value"] = subject
        code, output, errors = self.cli({"abc": msg}, ["--json", "abc"])
        self.assertEqual(code, 0)
        self.assertEqual(json.loads(output)[0]["subject"], subject)
        self.assertTrue(output.isascii())
        code, output, errors = self.cli({"abc": msg}, ["abc"])
        self.assertEqual(code, 0)
        self.assertEqual(len(output.splitlines()), 2)
        self.assertEqual(len(output.splitlines()[1].split("\t")), 8)
        self.assertNotIn("\x00", output)
        self.assertIn(r"\ud800", output)
        output.encode("utf-8")  # The actual terminal encoding must not fail.
        self.assertEqual(errors, "")

    def test_cli_help_does_not_call_any_service(self):
        code, output, errors = self.cli({}, ["--help"])
        self.assertEqual(code, 0)
        self.assertIn("--json", output)
        self.assertEqual(errors, "")


if __name__ == "__main__":
    unittest.main()
