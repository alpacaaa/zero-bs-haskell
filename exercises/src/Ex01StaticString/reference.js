const app = require("express")()

app.get("/hello", (req, res) => {
  res.send("hello")
})

console.log("[ex1] Starting server...")
app.listen(7879)