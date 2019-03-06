
const chai = require('chai')
const chaiHttp = require('chai-http')
const { spawn } = require('child_process')

chai.use(chaiHttp)
const should = chai.should()

const exercise = process.env.EXERCISE

const test1 = () => {
  describe('Books', () => {
    describe('/GET book', () => {
        it('it should GET all the books', (done) => {
          chai.request('http://localhost:7879')
              .get('/book')
              .end((err, res) => {
                  if (err) return done(err)
                    res.should.have.status(200);
                    res.body.should.be.a('array');
                    res.body.length.should.be.eql(0);
                done();
              });
        });
    });
    /*
    * Test the /POST route
    */
    /*
    describe('/POST book', () => {
        it('it should not POST a book without pages field', (done) => {
            let book = {
                title: "The Lord of the Rings",
                author: "J.R.R. Tolkien",
                year: 1954
            }
          chai.request('http://localhost:3001')
              .post('/book')
              .send(book)
              .end((err, res) => {
                  if (err) return done(err)
                    res.should.have.status(200);
                    res.body.should.be.a('object');
                    res.body.should.have.property('errors');
                    res.body.errors.should.have.property('pages');
                    res.body.errors.pages.should.have.property('kind').eql('required');
                done();
              });
        });

    });

    */
  })
}

switch (exercise) {
  case "1": return test1()
  default: console.log("wtf?")
}
