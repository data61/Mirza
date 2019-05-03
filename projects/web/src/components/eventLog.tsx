import * as React from "react";

import { Link } from "react-router-dom";
import { Panel } from "./panel";

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

export function EventLookup({ className }: { className: string }) {
  return (
    <div className={className}>
      <h4>Event Lookup</h4>
      <form>
        <fieldset>
          <label htmlFor="dateRange">Date Range</label>
          <input type="text" id="dateRange" />

          <label htmlFor="eventType">Event Type</label>
          <select id="eventType">
            <option></option>
            <option>Aggregation</option>
            <option>Transaction</option>
          </select>

          <label htmlFor="epcLabel">EPC Label</label>
          <input type="text" id="epcLabel" />

          <input className="button-primary" type="submit" value="Lookup Events" />
        </fieldset>
      </form>
    </div>
  );
}

export function Events() {
  return (
    <div>
      <div className="border-bottom pad-tb">
        <div className="container">
          <div className="row">
            <EventLookup className="column border-right"></EventLookup>
            <SavedEvents className="column"></SavedEvents>
          </div>
        </div>
      </div>
      <div className="pad-tb">
        <div className="container">
          <h4><i className="fas fa-fw fa-lg fa-list-alt"></i> Events</h4>
          <Panel></Panel>
          <Panel></Panel>
          <Panel></Panel>
          <Panel></Panel>
        </div>
      </div>
    </div >
  );
}

export function EventLog() {
  return (
    <section>
      <div className="border-bottom">
        <div className="container">
          <div className="row">
            <div className="column"><h3><Link to="/"><i className="fa fa-chevron-left"></i> </Link> Event Log</h3></div>
            <div className="column">
              <nav className="tabs">
                <a className="active" href="#"><i className="fas fa-fw fa-lg fa-list-alt"></i> Events</a>
                <a href="#"><i className="fas fa-fw fa-lg fa-map-marked-alt"></i> Map View</a>
                <a href="#"><i className="fas fa-fw fa-lg fa-search"></i> Search Events</a>
              </nav>
            </div>
          </div>
        </div>
      </div>
      <Events></Events>
    </section>
  );
}
