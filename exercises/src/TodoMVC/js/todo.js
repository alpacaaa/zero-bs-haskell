const Core = require("./Core")
const app = require("express")()

const createStore = currentState => {
  return cb => {
    return (req, res, next) => {
      const store = {
        state: currentState,
        update: newState => (currentState = newState)
      }

      cb(store, req, res)
    }
  }
}

const withState = createStore(Core.initialState)

const decodeInputOrFail = (state, req, cb) => {
  // something something req.body
}

const getAll = withState(({ state, update }, req, res) => {
  res.json(Core.stateToList(state))
})

const postTodo = withState(({ state, update }, req, res) => {
  decodeInputOrFail(state, req, input => {
    const [newState, newTodo] = Core.createTodo(state, input)
    update(newState)
    res.json(newTodo)
  })
})

// ------
const getAll = (state, _, next) => {
  next(state, jsonResponse(Core.stateToList(state)))
}

const postTodo = (state, req, next) => {
  decodeInputOrFail(state, req, input => {
    if (input.title === null) {
      next(state, failureResponse("Empty title"))
    } else {
      const [newState, newTodo] = Core.createTodo(state, input)
      next(newState, jsonResponse(newTodo))
    }
  })
}

const findTodoOrFail = (state, req, cb) => {
  let urlId = req.urlParam("id")

  if (urlId === null) {
    return [state, failureResponse("No :id found in URL")]
  } else {
    let todo = Core.findTodo(state, urlId)

    if (todo === null) {
      return [state, failureResponse("Todo not found")]
    } else {
      return cb(todo)
    }
  }
}

app.get("/api", getAll)
app.post("/api", postTodo)

Server.startServer([
  Server.handlersWithState(Core.initialState, [
    Server.statefulHandler(Server.GET, "/api", getAll),
    Server.statefulHandler(Server.POST, "/api", postTodo),
    Server.statefulHandler(Server.DELETE, "/api", deleteAll),
    Server.statefulHandler(Server.GET, "/api/:id", getTodo),
    Server.statefulHandler(Server.PATCH, "/api/:id", patchTodo),
    Server.statefulHandler(Server.DELETE, "/api/:id", deleteTodo)
  ])
])
