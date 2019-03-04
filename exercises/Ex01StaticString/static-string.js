
const app = require("express")()

app.get("/hello", (req, res) => {
  res.send("hello")
})

app.listen(7879)
