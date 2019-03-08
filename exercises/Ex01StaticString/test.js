
module.exports = (server) => {
  describe('Exercise 01 - Static String', () => {
    describe('GET /hello', () => {
      it('it should match `hello`', (done) => {
        server
        .get('/hello')
        .end((err, res) => {
          if (err) return done(err)
          res.should.have.status(200)
          res.text.should.equal('hello')
          done()
        })
      })
    })
  })
}
