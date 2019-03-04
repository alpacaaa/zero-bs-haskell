
const app = require("express")()
const bodyParser = require("body-parser")

app.use(bodyParser.raw({ type: "*/*" }))

app.post("/echo", (req, res) => {
  res.json(req.body.toString())
})

app.listen(7879)
