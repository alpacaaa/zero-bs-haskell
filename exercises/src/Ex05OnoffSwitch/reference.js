
const app = require("express")()

// Mutable state!
let currentState = "Off"

app.post("/onoff-switch", (req, res) => {
  currentState = currentState === "Off" ? "On" : "Off"
  res.send(currentState)
})

console.log("Starting server...")
app.listen(7879)
