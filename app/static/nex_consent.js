/**
 * NEx consent state and UI wiring — opt-out model.
 *
 * Two independently controllable categories, both ON by default:
 *   analytics -> Google Analytics 4, gated through Google Consent Mode v2
 *   metrics   -> NEx first-party usage metrics (usage_metrics.sqlite3)
 *
 * A visitor's selection lives in localStorage and reflected in a cookie
 * so the FastAPI middleware can honour it server-side.
 *
 * IMPORTANT: this file must execute *before* the gtag/js script tag.
 * Consent Mode only respects a default that reaches dataLayer before the tag
 * first fires; push it late and one granted-mode hit has already gone. main.R
 * inlines it with includeScript() for exactly that reason as an external
 * <script src> would race the async gtag fetch.
 *
 * The notice bar and privacy page are wired here too, rather than in their R modules.
 * Both are singletons (meaning they are initiated once), use plain element ids and
 * need no Shiny namespacing.
 */
(function () {
  "use strict";

  var STORE_KEY = "nex_consent";
  var METRICS_COOKIE = "nex_metrics";
  var USER_ID_KEY = "nex_user_id";
  var MAX_AGE = 31536000; // 1 year

  // Bump when the notice copy changes, or when a new data category starts being
  // collected: every visitor then sees the bar again.
  var NOTICE_VERSION = 1;

  // A opt-out dismissal also goes stale, so users see the notice every ~6 months
  var NOTICE_MAX_AGE_MS = 182 * 24 * 60 * 60 * 1000;

  // Production uses this host. Everything else (e.g. localhost, 127.0.0.1, host = "0.0.0.0") is dev
  // The notice always shows under dev, similar to dev banner
  var PRODUCTION_HOST = "neuromics-explorer.ukdri.ac.uk";

  function isDevHost() {
    return String(window.location.hostname).toLowerCase() !== PRODUCTION_HOST;
  }

  // ── Storage helpers ──────────────────────────────────────────────────
  // localStorage throws outright in some privacy modes, so every access is guarded.
  function get(key) {
    try {
      return window.localStorage.getItem(key);
    } catch (e) {
      return null;
    }
  }

  function set(key, value) {
    try {
      window.localStorage.setItem(key, value);
    } catch (e) {
      /* cannot persist; the choice still applies to this page view */
    }
  }

  function drop(key) {
    try {
      window.localStorage.removeItem(key);
    } catch (e) {
      /* nothing to remove */
    }
  }

  function cookie(name, value, maxAge) {
    document.cookie =
      name + "=" + value + "; path=/; samesite=lax; max-age=" + maxAge;
  }

  // ── Consent state ────────────────────────────────────────────────────
  // `seen` and `version` live in the same record as the choices rather than in a
  // separate key. One storage key means "Restore defaults" on the privacy page
  // genuinely restores the first-visit state, bar included, instead of resetting
  // the choices while leaving the notice suppressed.
  function read() {
    var state = { analytics: true, metrics: true, seen: 0, version: 0 };
    try {
      var saved = JSON.parse(get(STORE_KEY));
      if (typeof saved.analytics === "boolean") state.analytics = saved.analytics;
      if (typeof saved.metrics === "boolean") state.metrics = saved.metrics;
      if (typeof saved.seen === "number") state.seen = saved.seen;
      if (typeof saved.version === "number") state.version = saved.version;
    } catch (e) {
      /* absent or corrupt — keep the defaults */
    }
    return state;
  }

  var current = read();

  /** Record that the visitor has seen the current version of the notice. */
  function markSeen() {
    current.seen = Date.now();
    current.version = NOTICE_VERSION;
    set(STORE_KEY, JSON.stringify(current));
  }

  function shouldShowNotice() {
    // Outside production the notice is always shown, so a dismissal recorded in
    // one browser during testing never hides it again. Dismissing still works
    // for the current page view; a reload brings it back.
    if (isDevHost()) return true;

    // ?notice=show forces it in production too, without clearing stored state.
    if (/[?&]notice=show(&|$)/.test(window.location.search)) return true;

    if (!current.seen) return true;                      // never seen
    if (current.version !== NOTICE_VERSION) return true; // notice has changed
    return Date.now() - current.seen > NOTICE_MAX_AGE_MS; // dismissal is stale
  }

  function gtag() {
    window.dataLayer = window.dataLayer || [];
    window.dataLayer.push(arguments);
  }

  /** mode is "default" on first load, "update" for a later change. */
  function apply(mode) {
    gtag("consent", mode, {
      analytics_storage: current.analytics ? "granted" : "denied",
      // NEx runs no advertising or remarketing, so these are denied outright
      // rather than offered as a choice.
      ad_storage: "denied",
      ad_user_data: "denied",
      ad_personalization: "denied"
    });

    // The backend skips logging entirely when metrics are declined, so the
    // opt-out has to be visible to it on every request.
    if (current.metrics) {
      cookie(METRICS_COOKIE, "", 0);
    } else {
      cookie(METRICS_COOKIE, "off", MAX_AGE);
      // Drop the persistent identifier too; get_user_id.js falls back to a
      // sessionStorage value so nothing survives the tab closing.
      drop(USER_ID_KEY);
      cookie(USER_ID_KEY, "", 0);
    }

    // Shiny reads this to decide whether to send X-NEX-Metrics: off upstream.
    // The cookie header alone is captured once at websocket connect and goes
    // stale the moment someone changes their choice mid-session.
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue("nex_metrics_opt_out", !current.metrics, {
        priority: "event"
      });
    }

    if (mode === "update") {
      document.dispatchEvent(
        new CustomEvent("nex:consent-changed", { detail: window.nexConsent.get() })
      );
    }
  }

  window.nexConsent = {
    get: function () {
      return { analytics: current.analytics, metrics: current.metrics };
    },

    /** Apply a partial change, e.g. set({ analytics: false }). */
    set: function (changes) {
      if (typeof changes.analytics === "boolean") current.analytics = changes.analytics;
      if (typeof changes.metrics === "boolean") current.metrics = changes.metrics;
      // Acting on the choices means the notice has been seen, whether that
      // happened on the bar or on the privacy page.
      markSeen();
      apply("update");
      return this.get();
    },

    /** Discard the stored choice and return to the first-visit state. */
    reset: function () {
      drop(STORE_KEY);
      current = read();
      apply("update");
      var bar = document.getElementById("nex-notice");
      if (bar) bar.hidden = false;
      return this.get();
    },

    /** Dev helper: reveal the bar without touching stored state. */
    showNotice: function () {
      var bar = document.getElementById("nex-notice");
      if (bar) bar.hidden = false;
      return "Notice shown. nexConsent.reset() clears state for a true first-visit test.";
    }
  };

  apply("default");
  document.addEventListener("shiny:connected", function () {
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue("nex_metrics_opt_out", !current.metrics, {
        priority: "event"
      });
    }
  });

  // ── Notice bar ───────────────────────────────────────────────────────
  function wireNoticeBar() {
    var bar = document.getElementById("nex-notice");
    if (!bar) return;

    // bslib renders page_navbar's footer inside the tab-content wrapper, so a
    // bar left there is subject to that container's layout and stacking. Making
    // it a direct child of <body> is what guarantees position: fixed pins to the
    // viewport and paints above the page on every route, not just tall ones.
    document.body.appendChild(bar);

    var rest = document.getElementById("nex-notice-rest");
    var more = document.getElementById("nex-notice-more");

    function dismiss() {
      markSeen();
      bar.hidden = true;
    }

    bar.hidden = !shouldShowNotice();

    more.addEventListener("click", function () {
      var opening = rest.hidden;
      rest.hidden = !opening;
      more.textContent = opening ? "Less" : "More";
      more.setAttribute("aria-expanded", String(opening));
    });

    // Applies the opt-out and closes. No inline confirmation: a "you have opted
    // out" message sitting in the bar reads as though the choice was already
    // made for you, which is the opposite of what an opt-out notice should say.
    document.getElementById("nex-notice-optout").addEventListener("click", function () {
      window.nexConsent.set({ metrics: false, analytics: false });
      dismiss();
    });

    document.getElementById("nex-notice-gotit").addEventListener("click", dismiss);
    document.getElementById("nex-notice-close").addEventListener("click", dismiss);

    document.getElementById("nex-notice-privacy").addEventListener("click", function () {
      if (window.Shiny && window.Shiny.setInputValue) {
        window.Shiny.setInputValue("nex_open_privacy", Date.now(), { priority: "event" });
      }
      dismiss();
    });
  }

  // Persistent link in the navbar, so the privacy page stays reachable once the
  // bar has been dismissed.
  function wirePrivacyLink() {
    var link = document.getElementById("nex-privacy-navlink");
    if (!link) return;
    link.addEventListener("click", function () {
      if (window.Shiny && window.Shiny.setInputValue) {
        window.Shiny.setInputValue("nex_open_privacy", Date.now(), { priority: "event" });
      }
    });
  }

  // ── Privacy page ─────────────────────────────────────────────────────
  function wirePrivacyPage() {
    var metrics = document.getElementById("nex-privacy-metrics");
    var analytics = document.getElementById("nex-privacy-analytics");
    if (!metrics || !analytics) return;

    var status = document.getElementById("nex-privacy-status");
    var idField = document.getElementById("nex-privacy-id");
    var copyBtn = document.getElementById("nex-privacy-copy");
    var timer = null;

    function say(message) {
      status.textContent = message;
      window.clearTimeout(timer);
      timer = window.setTimeout(function () {
        status.textContent = "";
      }, 4000);
    }

    // Reflect stored state rather than the markup's default, so the switches
    // are correct for a returning visitor and after a change made elsewhere.
    function render() {
      var state = window.nexConsent.get();
      metrics.checked = state.metrics;
      analytics.checked = state.analytics;

      var stored = null;
      try {
        stored = get(USER_ID_KEY) || window.sessionStorage.getItem(USER_ID_KEY);
      } catch (e) {
        stored = null;
      }
      idField.textContent = stored || "None stored in this browser";
      copyBtn.disabled = !stored;
    }

    metrics.addEventListener("change", function () {
      window.nexConsent.set({ metrics: metrics.checked });
      say(
        metrics.checked
          ? "Usage metrics on."
          : "Usage metrics off. Nothing further is recorded."
      );
    });

    analytics.addEventListener("change", function () {
      window.nexConsent.set({ analytics: analytics.checked });
      say(analytics.checked ? "Google Analytics on." : "Google Analytics off.");
    });

    document.getElementById("nex-privacy-reset").addEventListener("click", function () {
      window.nexConsent.reset();
      say("Defaults restored.");
    });

    copyBtn.addEventListener("click", function () {
      if (!navigator.clipboard) return;
      navigator.clipboard.writeText(idField.textContent).then(function () {
        say("Identifier copied.");
      });
    });

    document.addEventListener("nex:consent-changed", render);
    render();
  }

  document.addEventListener("DOMContentLoaded", function () {
    wireNoticeBar();
    wirePrivacyPage();
    wirePrivacyLink();
  });
})();
