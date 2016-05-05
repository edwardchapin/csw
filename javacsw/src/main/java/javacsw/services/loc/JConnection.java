package javacsw.services.loc;

import akka.http.scaladsl.model.headers.Connection$;
import csw.services.loc.ComponentId;
import csw.services.loc.Connection;
import csw.services.loc.Connection.*;
import csw.services.loc.ConnectionType;

/**
 * Java API for location service connections
 */
public class JConnection {
    /**
     * A connection to a remote akka actor based component
     */
    public static AkkaConnection createAkkaConnection(ComponentId componentId) {
        return new AkkaConnection(componentId);
    }

    /**
     * A connection to a remote http based component
     */
    public static HttpConnection createHttpConnection(ComponentId componentId) {
        return new HttpConnection(componentId);
    }

    /**
     * Gets a Connection from a string as output by Connection.toString
     */
    public static Connection parseConnection(String s) {
        return csw.services.loc.Connection$.MODULE$.apply(s).get();
    }

    /**
     * Gets a Connection based on the component id and connection type
     */
    public static Connection createConnection(ComponentId componentId, ConnectionType connectionType) {
        return csw.services.loc.Connection$.MODULE$.apply(componentId, connectionType);
    }
}