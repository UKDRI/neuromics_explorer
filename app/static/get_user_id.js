/**
 * Get User ID if available otherwise use a randomised UUID.
 *
 * Consent-aware: the persistent identifier only exists while the visitor
 * allows first-party usage metrics (see nex_consent.js). If they have opted
 * out, no identifier is stored past the tab and none is handed to Shiny, so
 * the backend has nothing to key a log row on.
 */
const USER_ID_KEY = "nex_user_id";

function metricsAllowed() {
  // Opt-out model: absent consent state means allowed.
  return !window.nexConsent || window.nexConsent.get().metrics;
}

function newId() {
  // crypto.randomUUID is exposed only in secure contexts (http://localhost,
  // http://127.0.0.1 NOT LAN address host = "0.0.0.0") does not — so this path
  // Therefore path is reached when app is served with host = "0.0.0.0" and opened from
  // another device over http.
  if (window.crypto && typeof window.crypto.randomUUID === "function") {
    return window.crypto.randomUUID();
  }
  return "nex-" + Date.now().toString(36) + "-" + Math.random().toString(36).slice(2, 10);
}

export function getUserId() {
  if (!metricsAllowed()) {
    // This is a temporary id for the session which is not persistent, and generated on 
    // each page load. It is used to correlate events within a session, but not across sessions.
    let sessionId = sessionStorage.getItem(USER_ID_KEY);
    if (!sessionId) {
      sessionId = newId();
      sessionStorage.setItem(USER_ID_KEY, sessionId);
    }
    return sessionId;
  }

  let id = localStorage.getItem(USER_ID_KEY);
  if (!id) {
    id = newId();
    localStorage.setItem(USER_ID_KEY, id);
  }
  // Set cookie for servers that allow it (prevent race condition issue)
  document.cookie = `nex_user_id=${id}; path=/; samesite=lax; max-age=31536000`;

  // Push to Shiny (as input value "nex_user_id") once connected and ready, otherwise wait for connection
  if (window.Shiny?.setInputValue) {
    window.Shiny.setInputValue("nex_user_id", id, { priority: "event" });
  } else {
    document.addEventListener("shiny:connected", () =>
      window.Shiny.setInputValue("nex_user_id", getUserId(), { priority: "event" })
    );
  }

  return id;
}

// Re-run when the visitor changes their mind mid-session so the identifier is
// created or destroyed immediately rather than on the next page load.
document.addEventListener("nex:consent-changed", () => {
  getUserId();
});
