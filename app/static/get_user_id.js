/**
 * Get User ID if available otherwise use a randomised UUID.
 */
const USER_ID_KEY = "nex_user_id";

export function getUserId() {
  let id = localStorage.getItem(USER_ID_KEY);
  if (!id) {
    id = crypto.randomUUID();
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
