import * as React from "react";
import { Link } from "react-router-dom";
const { DigitalLink, Utils } = require('digital-link.js');

import { objectEvent } from "../epcis";
import { EventForm } from "./epcis/event";

export function Submit() {
  const eventState = React.useState(objectEvent());
  const [event, _] = eventState;
  const url = 'http://localhost:8020'

  const submitEvent = () => {
    console.log(event.label);
    const dl = DigitalLink(event.label);
    if(dl.isValid()) {
      const token = 'Bearer ' + JSON.parse(localStorage.getItem('auth0_tk'))['idToken']
      const request = new Request(url + '/event/objectEvent', {
        method: 'POST',
        body: JSON.stringify(event),
        headers: new Headers({
          'Accept': 'application/json',
          'Content-Type': 'application/json',
          'Authorization': token,
        }),
        credentials: 'include'
      });
      return fetch(request).then(function(res: Response) {
        console.log(res);
      }).catch(function(err) {
        console.error(err);
      })
    } else {
      alert("Invalid event.");
    }

    // TODO:
    // - Fetch data entity api URL from BR (based off user/selected company)
    // - Submit event to entity api URL
  };

  return (
    <section>
      <div className="border-bottom">
        <div className="container">
          <div className="row">
            <div className="column">
              <h3><Link to="/"><i className="fa fa-chevron-left"></i> </Link> New Event</h3>
            </div>
          </div>
        </div>
      </div>
      <div>
        <div className="border-bottom pad-tb">
          <div className="container">
            <div className="row">
              <div className="column border-right">
                <EventForm eventState={eventState} />

                <button onClick={submitEvent}>Submit Event</button>
              </div>
              <div className="column">
                <label>Raw EPCIS Event</label>
                <textarea style={({ height: "10em" })} value={JSON.stringify(event, null, 2)} readOnly></textarea>
              </div>
            </div>
          </div>
        </div>
      </div >
    </section >
  );
}
