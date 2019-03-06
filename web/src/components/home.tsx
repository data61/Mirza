import * as React from "react";

import { Link } from "react-router-dom";

export function Home() {
  return (
    <section>
      <div className="container">
        <div className="row">
          <div className="column">
            <Link to="/events">Scan</Link>
          </div>
          <div className="column">
            <Link to="/events">Event Log</Link>
          </div>
        </div>
      </div>
    </section>
  )
}
