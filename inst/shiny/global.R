library(rdrop2)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials <- data.frame(
  user = c("bscott", "mwoodward", "cmarang", "kpatwari"),
  password = c("bscott", "mwoodward", "cmarang", "kpatwari"),
  comment = c("logged in as bscott", "logged in as mwoodward", "logged in as cmarang", "logged in as kpatwari"),
  stringsAsFactors = FALSE
)

