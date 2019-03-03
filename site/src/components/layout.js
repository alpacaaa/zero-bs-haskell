import React from "react"
import PropTypes from "prop-types"
import { StaticQuery, graphql } from "gatsby"
import {Helmet} from "react-helmet"

import Header from "./header"
import "./layout.css"

const Layout = ({ children }) => (
  <StaticQuery
    query={graphql`
      query SiteTitleQuery {
        site {
          siteMetadata {
            title
          }
        }
      }
    `}
    render={data => (
      <>
        <Helmet>
          <link href="https://fonts.googleapis.com/css?family=Muli:300,400,700,800" rel="stylesheet" />
          <link href="https://fonts.googleapis.com/css?family=PT+Sans:400,700" rel="stylesheet" />
        </Helmet>

        <header class="main-header">
          <h1><a href="/" class="js-load"><strong>Zero Bullshit</strong> Haskell</a></h1>
          <ul role="nav">
            <li><a href="/about" class="js-nav-about js-load">Wisdom</a></li>
            <li><a href="/buy" class="js-buy-toggle">Hands on</a></li>
          </ul>
        </header>
        <div
          style={{
            marginTop: '4em'
          }}
        >
          {children}
          <footer>
            © {new Date().getFullYear()}, Built with
            {` `}
            <a href="https://www.gatsbyjs.org">Gatsby</a>
          </footer>
        </div>
      </>
    )}
  />
)

Layout.propTypes = {
  children: PropTypes.node.isRequired,
}

export default Layout
