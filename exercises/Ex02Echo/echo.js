
const app = require("express")()
const bodyParser = require("body-parser")

app.use(bodyParser.raw({ type: "*/*" }))

app.post("/echo", (req, res) => {
  res.send(req.body.toString())
})

console.log("Starting server...")
app.listen(7879)
