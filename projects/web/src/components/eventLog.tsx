import * as React from "react";
import { Link } from "react-router-dom";

import { myGlobals } from "../globals";

import { AuthState } from "../auth";
import { Organisation } from "../business-registry";
import { Panel } from "./panel";

import { PrettyEventEPCIS } from "../epcis";
import { queryForm } from "../query";

import {
  GoogleMap,
  Marker,
  withGoogleMap,
  withScriptjs,
} from "react-google-maps";

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

export interface MapProps {
  center: {
    lat: number;
    lng: number;
  };
  orgName: string;
  zoom: number;
  markers: Array<{
    address: string,
    coord: {
      lat: number;
      lng: number;
    }
  }>;
}

function defaultMapProps(): MapProps {
  return {
    center: {
      // Co-ordinates of Sydney, NSW, AU
      lat: -33.865143,
      lng: 151.2093,
    },
    zoom: 8,
    orgName: '',
    markers: [],
  };
}

const googleMapURL = !myGlobals.googleMapsApiKey ? null : encodeURI(
    "https://maps.googleapis.com/maps/api/js?key=" +
    myGlobals.googleMapsApiKey +
    "&v=3.exp&libraries=geometry,drawing,places"
  );

const MapWithMarker = withScriptjs(withGoogleMap((props: MapProps) => {
  if(!props.markers) {return null;}
  return (
    <GoogleMap
      defaultZoom={props.zoom}
      defaultCenter={props.center}
    >
      {
        props.markers.map((prettyEvent, index) => {
          if(prettyEvent.coord && prettyEvent.address) {
            return (
              <Marker
                key={index}
                title={prettyEvent.address}
                position={prettyEvent.coord}
              ></Marker>
            );
          }
          return null;
        })
      }
    </GoogleMap>
  )
}));

export function EventLookup(props: QueryProps) {

  const [query, queryUpdate] = React.useState(queryForm());
  const [mapsProp, mapsPropUpdate] = React.useState(defaultMapProps());
  const [eventRes, eventResSet] = React.useState(null);
  const queryEvent = () => {
    return fetch(encodeURI(props.organisation.url + '/events/prototype/list/events/' + query.Label), {
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
              {eventRes.length > 0 ? eventRes.map((ev: PrettyEventEPCIS, index: number) => {
                return <Panel key={index} event={ev}></Panel>;
              }) : <p>No Results</p>
              }
            </div>
            <div style={{ height: '100vh', width: '100%' }}>
              <MapWithMarker
                googleMapURL={googleMapURL}
                loadingElement={<div style={{ height: `100%` }} />}
                containerElement={<div style={{ height: `400px` }} />}
                mapElement={<div style={{ height: `100%` }} />}
                center={mapsProp.center}
                orgName={mapsProp.orgName}
                zoom={mapsProp.zoom}
                markers={eventRes}
              />
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
