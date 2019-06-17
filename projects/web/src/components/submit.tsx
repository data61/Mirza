import * as React from "react";
import { Link } from "react-router-dom";

import { objectEvent } from "../epcis";
import { EventForm } from "./epcis/event";

const { DigitalLink } = require("digital-link.js");
const {edapiUrl, orUrl} = require("../globals").myGlobals;

export function Submit() {
  const eventState = React.useState(objectEvent());

  const submitEvent = () => {
    const token = 'Bearer ' + JSON.parse(localStorage.getItem('auth0_tk'))['idToken']
    return fetch(new Request(orUrl + '/user/orgs', {
      method: 'GET',
      headers: new Headers({
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Authorization': token,
      }),
      credentials: 'include',
    }
    )).then(function(res: Response) {
      return res.json();
    }).then(function(data) {
      if (data[0]) {
        return data[0].url;
      }
      return Promise.resolve();
    }).then(function(url) {
      const [event, _] = eventState;
      const dl = DigitalLink(event.epcList[0]);
      if (!dl.isValid()) {
        return Promise.reject("Invalid GS1 Label");
      }
      event.epcList[0] = dl.mapToGS1Urn();
      console.log(dl.mapToGS1Urn());
      console.log(JSON.stringify(event));
      const request = new Request(url + '/event', {
        method: 'POST',
        body: JSON.stringify(event),
        headers: new Headers({
          'Accept': 'application/json',
          'Content-Type': 'application/json',
          'Authorization': token,
        }),
        credentials: 'include',
      });
      return fetch(request);
    }).then(function(res: Response) {
      if (res.status === 200) {
        alert('Success!');
      } else {
        alert('Failed with status: ' + res.status);
      }
    }).catch(function(err) {
      console.log(err);
    });

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
