import * as React from "react";
import { Link } from "react-router-dom";

import { objectEvent } from "../epcis";
import { EventForm } from "./epcis/event";

export function Submit() {
  const eventState = React.useState(objectEvent());
  const [event, _] = eventState;

  return (
    <section>
      <div className="border-bottom">
        <div className="container">
          <div className="row">
            <div className="column">
              <h3><Link to="/"><i className="fa fa-chevron-left"></i> </Link> Submit Event</h3>
            </div>
            <div className="column">
              <nav className="tabs">
                <a className="active" href="#"><i className="fas fa-fw fa-lg fa-qrcode"></i> Scan</a>
              </nav>
            </div>
          </div>
        </div>
      </div>
      <div>
        <div className="border-bottom pad-tb">
          <div className="container">
            <div className="row">
              <div className="column border-right"><EventForm eventState={eventState} /></div>
              <div className="column">
                <textarea style={({ height: "100%" })} value={JSON.stringify(event, null, 2)} readOnly></textarea>
              </div>
            </div>
          </div>
        </div>
      </div >
    </section>
  );
}
