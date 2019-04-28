
module.exports = (server) => {
  const shouldMatch = (expected) => {
    it(`it should match "${JSON.stringify(expected)}"`, (done) => {
      server
      .get('/cart')
      .end((err, res) => {
        if (err) return done(err)
        res.should.have.status(200)
        res.body.should.eql(expected)
        done()
      })
    })
  }

  const addItem = (item) => {
    it(`it should add an item to the cart`, (done) => {
      server
      .post('/cart', item)
      .end((err, res) => {
        if (err) return done(err)
        res.should.have.status(200)
        done()
      })
    })
  }

  describe('Exercise 08 - ShoppingCart V2', () => {
    const item = (model, quantity) => { return { model, quantity }}
    const stuff = item('stuff', 1)
    const crap = item('crap no one needs', 2)
    const toilet = item('toilet paper', 5)

    shouldMatch([])
    addItem(stuff)
    shouldMatch([stuff])
    addItem(crap)
    shouldMatch([crap, stuff])
    addItem(item('stuff', 6))
    shouldMatch([item('stuff', 7), crap])
    addItem(toilet)
    shouldMatch([item('stuff', 7), toilet, crap])

  })
}

