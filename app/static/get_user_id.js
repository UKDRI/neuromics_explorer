/**
 * Get User ID if available otherwise use a randomised UUID.
 */
const USER_ID_KEY = "nex_user_id";

function getUserId() {
  let id = localStorage.getItem(USER_ID_KEY);
  if (!id) {
    id = crypto.randomUUID();
    localStorage.setItem(USER_ID_KEY, id);
  }
  return id;
}


/* Push to Shiny once connected and ready, otherwise wait for connection
 * then push user ID to Shiny input value "nex_user_id"
 */
function sendToShiny(id) {
  if (window.Shiny?.setInputValue) {
    window.Shiny.setInputValue("nex_user_id", id, { priority: "event" });
  } else {
    document.addEventListener("shiny:connected", () =>
      window.Shiny.setInputValue("nex_user_id", getUserId(), { priority: "event" })
    );
  }
}

sendToShiny(getUserId());