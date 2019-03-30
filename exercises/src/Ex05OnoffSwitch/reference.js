
const app = require("express")()
const bodyParser = require("body-parser")

// Mutable state!
let currentState = "Off"

app.post("/onoff-switch", (req, res) => {
  currentState = currentState === "Off" ? "On" : "Off"
  res.send(currentState)
})

console.log("Starting server...")
app.listen(7879)
