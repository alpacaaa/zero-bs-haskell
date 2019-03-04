
const app = require("express")()

app.get("/hello", (req, res) => {
  res.json("hello")
})

app.listen(7879)
