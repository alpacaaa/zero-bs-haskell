
const app = require("express")()
const bodyParser = require("body-parser")

app.use(bodyParser.raw({ type: "*/*" }))

app.post("/string-manipulation", (req, res) => {
  let result
  const body = req.body.toString()
  const search = "I'm positive"

  if (body.slice(0, search.length) === search) {
    result = "I think" + body.substr(search.length)
  }
  else {
    result = body
  }

  res.send(result)
})

console.log("Starting server...")
app.listen(7879)
