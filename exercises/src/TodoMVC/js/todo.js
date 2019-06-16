const Core = require("./Core")
const HTTP = require("./HTTP")

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

const deleteAll = (state, _, next) => {
  next(Core.initialState, jsonResponse("ok"))
}

const getTodo = (state, req, next) => {
  findTodoOrFail(state, req, todo => {
    next(state, jsonResponse(todo))
  })
}

const deleteTodo = (state, req, next) => {
  findTodoOrFail(state, req, todo => {
    next(Core.deleteTodo(state, todo), jsonResponse("ok"))
  })
}

const patchTodo = (state, req, next) => {
  findTodoOrFail(state, req, existing => {
    decodeInputOrFail(state, req, input => {
      let [newState, updated] = Core.updateTodo(
        state,
        existing,
        input.title,
        input.completed,
        input.order
      )
      next(newState, jsonResponse(updated))
    })
  })
}

const decodeInputOrFail = (state, req, cb) => {
  // something something req.body
  cb(req.body)
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

HTTP.startServer(Core.initialState, [
  HTTP.get("/api", getAll),
  HTTP.post("/api", postTodo),
  HTTP.delete("/api", deleteAll),
  HTTP.get("/api/:id", getTodo),
  HTTP.patch("/api/:id", patchTodo),
  HTTP.delete("/api/:id", deleteTodo)
])
