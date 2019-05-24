import { stringify } from "query-string";

export const EventType = {
    Aggregation: "AggregationEvent",
    Object: "ObjectEvent",
    Transaction: "TransactionEvent",
    Transformation: "TransformationEvent",
};

export const EventAction = {
    Add: "ADD",
    Delete: "DELETE",
    Observe: "OBSERVE",
};

export const EventBusinessStep = {
    Accepting: "urn:epcglobal:cbv:bizstep:accepting",
    Arriving: "urn:epcglobal:cbv:bizstep:arriving",
    Assembling: "urn:epcglobal:cbv:bizstep:assembling",
    Collecting: "urn:epcglobal:cbv:bizstep:collecting",
    Commissioning: "urn:epcglobal:cbv:bizstep:commissioning",
    Consigning: "urn:epcglobal:cbv:bizstep:consigning",
    CreatingClassInstance: "urn:epcglobal:cbv:bizstep:creating_class_instance",
    CycleCounting: "urn:epcglobal:cbv:bizstep:cycle_counting",
    Decommissioning: "urn:epcglobal:cbv:bizstep:decommissioning",
    Departing: "urn:epcglobal:cbv:bizstep:departing",
    Destroying: "urn:epcglobal:cbv:bizstep:destroying",
    Disassembling: "urn:epcglobal:cbv:bizstep:disassembling",
    Dispensing: "urn:epcglobal:cbv:bizstep:dispensing",
    Encoding: "urn:epcglobal:cbv:bizstep:encoding",
    EnteringExiting: "urn:epcglobal:cbv:bizstep:entering_exiting",
    Holding: "urn:epcglobal:cbv:bizstep:holding",
    Inspecting: "urn:epcglobal:cbv:bizstep:inspecting",
    Installing: "urn:epcglobal:cbv:bizstep:installing",
    Killing: "urn:epcglobal:cbv:bizstep:killing",
    Loading: "urn:epcglobal:cbv:bizstep:loading",
    Other: "urn:epcglobal:cbv:bizstep:other",
    Packing: "urn:epcglobal:cbv:bizstep:packing",
    Picking: "urn:epcglobal:cbv:bizstep:picking",
    Receiving: "urn:epcglobal:cbv:bizstep:receiving",
    Removing: "urn:epcglobal:cbv:bizstep:removing",
    Repackaging: "urn:epcglobal:cbv:bizstep:repackaging",
    Repairing: "urn:epcglobal:cbv:bizstep:repairing",
    Replacing: "urn:epcglobal:cbv:bizstep:replacing",
    Reserving: "urn:epcglobal:cbv:bizstep:reserving",
    RetailSelling: "urn:epcglobal:cbv:bizstep:retail_selling",
    Shipping: "urn:epcglobal:cbv:bizstep:shipping",
    StagingOutbound: "urn:epcglobal:cbv:bizstep:staging_outbound",
    StockTaking: "urn:epcglobal:cbv:bizstep:stock_taking",
    Stocking: "urn:epcglobal:cbv:bizstep:stocking",
    Storing: "urn:epcglobal:cbv:bizstep:storing",
    Transporting: "urn:epcglobal:cbv:bizstep:transporting",
    Unloading: "urn:epcglobal:cbv:bizstep:unloading",
    VoidShipping: "urn:epcglobal:cbv:bizstep:void_shipping",
};

export const EventDisposition = {
    Active: "urn:epcglobal:cbv:disp:active",
    ContainerClosed: "urn:epcglobal:cbv:disp:container_closed",
    Damaged: "urn:epcglobal:cbv:disp:damaged",
    Destroyed: "urn:epcglobal:cbv:disp:destroyed",
    Dispensed: "urn:epcglobal:cbv:disp:dispensed",
    Disposed: "urn:epcglobal:cbv:disp:disposed",
    Encoded: "urn:epcglobal:cbv:disp:encoded",
    Expired: "urn:epcglobal:cbv:disp:expired",
    InProgress: "urn:epcglobal:cbv:disp:in_progress",
    InTransit: "urn:epcglobal:cbv:disp:in_transit",
    Inactive: "urn:epcglobal:cbv:disp:inactive",
    NoPedigreeMatch: "urn:epcglobal:cbv:disp:no_pedigree_match",
    NonSellableOther: "urn:epcglobal:cbv:disp:non_sellable_other",
    PartiallyDispensed: "urn:epcglobal:cbv:disp:partially_dispensed",
    Recalled: "urn:epcglobal:cbv:disp:recalled",
    Reserved: "urn:epcglobal:cbv:disp:reserved",
    RetailSold: "urn:epcglobal:cbv:disp:retail_sold",
    Returned: "urn:epcglobal:cbv:disp:returned",
    SellableAccessible: "urn:epcglobal:cbv:disp:sellable_accessible",
    SellableNotAccessible: "urn:epcglobal:cbv:disp:sellable_not_accessible",
    Stolen: "urn:epcglobal:cbv:disp:stolen",
    Unknown: "urn:epcglobal:cbv:disp:unknown",
};

export interface Event {
    isA: string;
    eventTime: Date;
    eventTimeZoneOffset: number;
    action?: string;
    epcList?: Array<string>;
    bizStep?: string;
    disposition?: string;
}

export function objectEvent(): Event {
    return {
        isA: EventType.Object,
        epcList: [],
        action: EventAction.Add,
        bizStep: EventBusinessStep.Accepting,
        disposition: EventDisposition.Active,
        eventTime: new Date(),
        eventTimeZoneOffset: (new Date()).getTimezoneOffset()
    };
}
