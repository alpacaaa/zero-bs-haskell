
const app = require("express")()

// Mutable state!
let currentState = 0

app.post("/increase", (req, res) => {
  currentState++
  res.send("ok")
})

app.get("/current-count", (req, res) => {
  res.send('' + currentState)
})

console.log("Starting server...")
app.listen(7879)
