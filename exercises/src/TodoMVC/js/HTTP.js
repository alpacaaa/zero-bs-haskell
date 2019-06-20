const express = require("express")
const cors = require("cors")

const createStore = currentState => {
  return {
    state: () => currentState,
    update: newState => {
      console.log("Updating state")
      console.log(newState)
      currentState = newState
    }
  }
}

const httpHandler = method => (path, handler) => (app, store) => {
  app[method](path, (rawReq, res, next) => {
    const req = { body: rawReq.body, params: rawReq.params } // todo
    const [newState, response] = handler(store.state(), req)

    store.update(newState)

    if (response.type === "json") {
      return res.json(response.body)
    }

    if (response.type === "failure") {
      return res.status(500).send(response.error)
    }

    res.json("WTF")
  })
}

const startServer = (initialState, handlers) => {
  const app = express()
  const store = createStore(initialState)

  app.use(express.json())
  app.use(cors())
  handlers.forEach(h => h(app, store))

  console.log("Starting server...")
  app.listen(7879)
}

const jsonResponse = json => ({ type: "json", body: json })
const failureResponse = err => ({ type: "failure", error: err })

// A bit stupid, but needed to keep compatibility with Haskell api
const stringResponse = jsonResponse

const requestParameter = (req, param) => req.params[param]

module.exports = {
  get: httpHandler("get"),
  post: httpHandler("post"),
  patch: httpHandler("patch"),
  delete: httpHandler("delete"),
  startServer,
  Server: {
    jsonResponse,
    stringResponse,
    failureResponse,
    requestParameter
  }
}
