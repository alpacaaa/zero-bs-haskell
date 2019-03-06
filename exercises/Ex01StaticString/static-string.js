
const app = require("express")()

app.get("/hello", (req, res) => {
  res.send("hello")
})

console.log("Starting server...")
app.listen(7879)
