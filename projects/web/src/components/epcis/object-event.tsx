import * as React from "react";

import { Event } from "../../epcis";
import { EventAction, EventBusinessStep, EventDisposition } from "../../epcis";
import { EventStateProps, LabelField, MappingSelect } from "./common";

export function ObjectEventForm({ state }: EventStateProps<Event>) {
  const [event, setEvent] = state;

  return (
    <fieldset>
      <LabelField state={state}
        updateFn={(e, v) => ({ ...e, epcList: [v] })}
        getFn={(e) => e.epcList.length > 0 ? e.epcList[0] : ""}
      />

      <label>
        <span>Action</span>
        <MappingSelect
          mapping={EventAction}
          value={event.action}
          setValue={(x: string) => setEvent({ ...event, action: x })} />
      </label>

      <label >
        <span>Business Step </span>
        <MappingSelect
          mapping={EventBusinessStep}
          value={event.bizStep}
          setValue={(x: string) => setEvent({ ...event, bizStep: x })} />
      </label>

      <label>
        <span>Disposition </span>
        <MappingSelect
          mapping={EventDisposition}
          value={event.disposition}
          setValue={(x: string) => setEvent({ ...event, disposition: x })} />
      </label>

    </fieldset>
  );
}
