import * as React from "react";
import { Link } from "react-router-dom";

import { AuthState } from "../auth";
import { Organisation } from "../business-registry";
import { Panel } from "./panel";

import { EventEPCIS } from "../epcis";
import { queryForm } from "../query";

export function SavedEvents({ className }: { className: string }) {
  return (
    <div className={className}>
      <h4>Saved Events</h4>
      <div className="button-list">
        <a className="button button-outline">Daily view - Dispatch</a>
        <a className="button button-outline">Daily view - Orders</a>
        <a className="button button-outline">Weekly view - Dispatch</a>
        <a className="button button-outline">Weekly view - Dispatch</a>
        <a className="button button-outline">Daily view - Purchases</a>
        <a className="button button-outline">Daily view - Payment</a>
        <a className="button button-outline">Weekly view - Purchases</a>
        <a className="button button-outline">Weekly view - Payment</a>
      </div>
    </div>
  );
}

export interface QueryProps {
  authState: AuthState;
  organisation: Organisation;
}

export function EventLookup(props: QueryProps) {

  const [query, queryUpdate] = React.useState(queryForm());
  const [eventRes, eventResSet] = React.useState(null);
  const queryEvent = () => {
    return fetch(encodeURI(props.organisation.url + '/epc/events/' + query.Label), {
      method: 'GET',
      headers: new Headers({
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Authorization': 'Bearer ' + props.authState.getToken().idToken,
      }),
    }).then(function(res: Response) {
      return res.json();
    }).then(function(data) {
      eventResSet(data);
      return Promise.resolve();
    }).catch(function(err) {
      console.log(err);
      return Promise.reject(new Error("An error occured"));
    });
  };

  return (
    <div>
      <div>
        <div className="border-bottom pad-tb">
          <div className="container">
            <h3>Event Lookup</h3>
            <form>
              <fieldset>
                <label htmlFor="epcLabel">EPC Label</label>
                <input name="epcLabel" type="text" id="epcLabel"
                  onChange={(e) => query.Label = e.target.value} />
              </fieldset>
            </form>
            <button onClick={queryEvent}>Lookup Events</button>
          </div>
        </div>
        {eventRes &&
          <div className="pad-tb">
            <div className="container">
              <h4><i className="fas fa-fw fa-lg fa-list-alt"></i> Events</h4>
              {eventRes.length > 0 ? eventRes.map((ev: EventEPCIS, index: number) => {
                return <Panel key={index} event={ev}></Panel>;
              }) : <p>No Results</p>
              }
            </div>
          </div>
        }
      </div >
    </div>
  );
}

export function EventLog(props: QueryProps) {
  return (
    <section>
      <div className="border-bottom">
        <div className="container">
          <h3><Link to="/"><i className="fa fa-chevron-left"></i> </Link></h3>
        </div>
      </div>
      <EventLookup authState={props.authState} organisation={props.organisation}></EventLookup>
    </section>
  );
}
