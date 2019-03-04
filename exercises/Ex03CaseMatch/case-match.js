
const app = require("express")()
const bodyParser = require("body-parser")

app.use(bodyParser.raw({ type: "*/*" }))

app.post("/case", (req, res) => {
  let result

  switch (req.body.toString()) {
    case "1":
      result = "one"
      break
    case "2":
      result = "two"
      break
    case "3":
      result = "three"
      break
    default:
      result = "What am I, a mathematician?"
  }

  res.send(result)
})

app.listen(7879)
