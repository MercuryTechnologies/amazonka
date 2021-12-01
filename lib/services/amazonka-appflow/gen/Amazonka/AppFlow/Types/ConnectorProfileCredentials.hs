{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppFlow.Types.ConnectorProfileCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorProfileCredentials where

import Amazonka.AppFlow.Types.AmplitudeConnectorProfileCredentials
import Amazonka.AppFlow.Types.DatadogConnectorProfileCredentials
import Amazonka.AppFlow.Types.DynatraceConnectorProfileCredentials
import Amazonka.AppFlow.Types.GoogleAnalyticsConnectorProfileCredentials
import Amazonka.AppFlow.Types.HoneycodeConnectorProfileCredentials
import Amazonka.AppFlow.Types.InforNexusConnectorProfileCredentials
import Amazonka.AppFlow.Types.MarketoConnectorProfileCredentials
import Amazonka.AppFlow.Types.RedshiftConnectorProfileCredentials
import Amazonka.AppFlow.Types.SAPODataConnectorProfileCredentials
import Amazonka.AppFlow.Types.SalesforceConnectorProfileCredentials
import Amazonka.AppFlow.Types.ServiceNowConnectorProfileCredentials
import Amazonka.AppFlow.Types.SingularConnectorProfileCredentials
import Amazonka.AppFlow.Types.SlackConnectorProfileCredentials
import Amazonka.AppFlow.Types.SnowflakeConnectorProfileCredentials
import Amazonka.AppFlow.Types.TrendmicroConnectorProfileCredentials
import Amazonka.AppFlow.Types.VeevaConnectorProfileCredentials
import Amazonka.AppFlow.Types.ZendeskConnectorProfileCredentials
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific credentials required by a connector.
--
-- /See:/ 'newConnectorProfileCredentials' smart constructor.
data ConnectorProfileCredentials = ConnectorProfileCredentials'
  { -- | The connector-specific credentials required when using Snowflake.
    snowflake :: Prelude.Maybe SnowflakeConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Amazon Honeycode.
    honeycode :: Prelude.Maybe HoneycodeConnectorProfileCredentials,
    -- | The connector-specific credentials required when using ServiceNow.
    serviceNow :: Prelude.Maybe ServiceNowConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Dynatrace.
    dynatrace :: Prelude.Maybe DynatraceConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Marketo.
    marketo :: Prelude.Maybe MarketoConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Slack.
    slack :: Prelude.Maybe SlackConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Singular.
    singular :: Prelude.Maybe SingularConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Infor Nexus.
    inforNexus :: Prelude.Maybe InforNexusConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Amplitude.
    amplitude :: Prelude.Maybe AmplitudeConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Datadog.
    datadog :: Prelude.Maybe DatadogConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Google Analytics.
    googleAnalytics :: Prelude.Maybe GoogleAnalyticsConnectorProfileCredentials,
    sAPOData :: Prelude.Maybe SAPODataConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Salesforce.
    salesforce :: Prelude.Maybe SalesforceConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Zendesk.
    zendesk :: Prelude.Maybe ZendeskConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Trend Micro.
    trendmicro :: Prelude.Maybe TrendmicroConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Amazon Redshift.
    redshift :: Prelude.Maybe RedshiftConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Veeva.
    veeva :: Prelude.Maybe VeevaConnectorProfileCredentials
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snowflake', 'connectorProfileCredentials_snowflake' - The connector-specific credentials required when using Snowflake.
--
-- 'honeycode', 'connectorProfileCredentials_honeycode' - The connector-specific credentials required when using Amazon Honeycode.
--
-- 'serviceNow', 'connectorProfileCredentials_serviceNow' - The connector-specific credentials required when using ServiceNow.
--
-- 'dynatrace', 'connectorProfileCredentials_dynatrace' - The connector-specific credentials required when using Dynatrace.
--
-- 'marketo', 'connectorProfileCredentials_marketo' - The connector-specific credentials required when using Marketo.
--
-- 'slack', 'connectorProfileCredentials_slack' - The connector-specific credentials required when using Slack.
--
-- 'singular', 'connectorProfileCredentials_singular' - The connector-specific credentials required when using Singular.
--
-- 'inforNexus', 'connectorProfileCredentials_inforNexus' - The connector-specific credentials required when using Infor Nexus.
--
-- 'amplitude', 'connectorProfileCredentials_amplitude' - The connector-specific credentials required when using Amplitude.
--
-- 'datadog', 'connectorProfileCredentials_datadog' - The connector-specific credentials required when using Datadog.
--
-- 'googleAnalytics', 'connectorProfileCredentials_googleAnalytics' - The connector-specific credentials required when using Google Analytics.
--
-- 'sAPOData', 'connectorProfileCredentials_sAPOData' - Undocumented member.
--
-- 'salesforce', 'connectorProfileCredentials_salesforce' - The connector-specific credentials required when using Salesforce.
--
-- 'zendesk', 'connectorProfileCredentials_zendesk' - The connector-specific credentials required when using Zendesk.
--
-- 'trendmicro', 'connectorProfileCredentials_trendmicro' - The connector-specific credentials required when using Trend Micro.
--
-- 'redshift', 'connectorProfileCredentials_redshift' - The connector-specific credentials required when using Amazon Redshift.
--
-- 'veeva', 'connectorProfileCredentials_veeva' - The connector-specific credentials required when using Veeva.
newConnectorProfileCredentials ::
  ConnectorProfileCredentials
newConnectorProfileCredentials =
  ConnectorProfileCredentials'
    { snowflake =
        Prelude.Nothing,
      honeycode = Prelude.Nothing,
      serviceNow = Prelude.Nothing,
      dynatrace = Prelude.Nothing,
      marketo = Prelude.Nothing,
      slack = Prelude.Nothing,
      singular = Prelude.Nothing,
      inforNexus = Prelude.Nothing,
      amplitude = Prelude.Nothing,
      datadog = Prelude.Nothing,
      googleAnalytics = Prelude.Nothing,
      sAPOData = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      zendesk = Prelude.Nothing,
      trendmicro = Prelude.Nothing,
      redshift = Prelude.Nothing,
      veeva = Prelude.Nothing
    }

-- | The connector-specific credentials required when using Snowflake.
connectorProfileCredentials_snowflake :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe SnowflakeConnectorProfileCredentials)
connectorProfileCredentials_snowflake = Lens.lens (\ConnectorProfileCredentials' {snowflake} -> snowflake) (\s@ConnectorProfileCredentials' {} a -> s {snowflake = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Amazon Honeycode.
connectorProfileCredentials_honeycode :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe HoneycodeConnectorProfileCredentials)
connectorProfileCredentials_honeycode = Lens.lens (\ConnectorProfileCredentials' {honeycode} -> honeycode) (\s@ConnectorProfileCredentials' {} a -> s {honeycode = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using ServiceNow.
connectorProfileCredentials_serviceNow :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe ServiceNowConnectorProfileCredentials)
connectorProfileCredentials_serviceNow = Lens.lens (\ConnectorProfileCredentials' {serviceNow} -> serviceNow) (\s@ConnectorProfileCredentials' {} a -> s {serviceNow = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Dynatrace.
connectorProfileCredentials_dynatrace :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe DynatraceConnectorProfileCredentials)
connectorProfileCredentials_dynatrace = Lens.lens (\ConnectorProfileCredentials' {dynatrace} -> dynatrace) (\s@ConnectorProfileCredentials' {} a -> s {dynatrace = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Marketo.
connectorProfileCredentials_marketo :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe MarketoConnectorProfileCredentials)
connectorProfileCredentials_marketo = Lens.lens (\ConnectorProfileCredentials' {marketo} -> marketo) (\s@ConnectorProfileCredentials' {} a -> s {marketo = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Slack.
connectorProfileCredentials_slack :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe SlackConnectorProfileCredentials)
connectorProfileCredentials_slack = Lens.lens (\ConnectorProfileCredentials' {slack} -> slack) (\s@ConnectorProfileCredentials' {} a -> s {slack = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Singular.
connectorProfileCredentials_singular :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe SingularConnectorProfileCredentials)
connectorProfileCredentials_singular = Lens.lens (\ConnectorProfileCredentials' {singular} -> singular) (\s@ConnectorProfileCredentials' {} a -> s {singular = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Infor Nexus.
connectorProfileCredentials_inforNexus :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe InforNexusConnectorProfileCredentials)
connectorProfileCredentials_inforNexus = Lens.lens (\ConnectorProfileCredentials' {inforNexus} -> inforNexus) (\s@ConnectorProfileCredentials' {} a -> s {inforNexus = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Amplitude.
connectorProfileCredentials_amplitude :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe AmplitudeConnectorProfileCredentials)
connectorProfileCredentials_amplitude = Lens.lens (\ConnectorProfileCredentials' {amplitude} -> amplitude) (\s@ConnectorProfileCredentials' {} a -> s {amplitude = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Datadog.
connectorProfileCredentials_datadog :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe DatadogConnectorProfileCredentials)
connectorProfileCredentials_datadog = Lens.lens (\ConnectorProfileCredentials' {datadog} -> datadog) (\s@ConnectorProfileCredentials' {} a -> s {datadog = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Google Analytics.
connectorProfileCredentials_googleAnalytics :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe GoogleAnalyticsConnectorProfileCredentials)
connectorProfileCredentials_googleAnalytics = Lens.lens (\ConnectorProfileCredentials' {googleAnalytics} -> googleAnalytics) (\s@ConnectorProfileCredentials' {} a -> s {googleAnalytics = a} :: ConnectorProfileCredentials)

-- | Undocumented member.
connectorProfileCredentials_sAPOData :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe SAPODataConnectorProfileCredentials)
connectorProfileCredentials_sAPOData = Lens.lens (\ConnectorProfileCredentials' {sAPOData} -> sAPOData) (\s@ConnectorProfileCredentials' {} a -> s {sAPOData = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Salesforce.
connectorProfileCredentials_salesforce :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe SalesforceConnectorProfileCredentials)
connectorProfileCredentials_salesforce = Lens.lens (\ConnectorProfileCredentials' {salesforce} -> salesforce) (\s@ConnectorProfileCredentials' {} a -> s {salesforce = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Zendesk.
connectorProfileCredentials_zendesk :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe ZendeskConnectorProfileCredentials)
connectorProfileCredentials_zendesk = Lens.lens (\ConnectorProfileCredentials' {zendesk} -> zendesk) (\s@ConnectorProfileCredentials' {} a -> s {zendesk = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Trend Micro.
connectorProfileCredentials_trendmicro :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe TrendmicroConnectorProfileCredentials)
connectorProfileCredentials_trendmicro = Lens.lens (\ConnectorProfileCredentials' {trendmicro} -> trendmicro) (\s@ConnectorProfileCredentials' {} a -> s {trendmicro = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Amazon Redshift.
connectorProfileCredentials_redshift :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe RedshiftConnectorProfileCredentials)
connectorProfileCredentials_redshift = Lens.lens (\ConnectorProfileCredentials' {redshift} -> redshift) (\s@ConnectorProfileCredentials' {} a -> s {redshift = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Veeva.
connectorProfileCredentials_veeva :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe VeevaConnectorProfileCredentials)
connectorProfileCredentials_veeva = Lens.lens (\ConnectorProfileCredentials' {veeva} -> veeva) (\s@ConnectorProfileCredentials' {} a -> s {veeva = a} :: ConnectorProfileCredentials)

instance Prelude.Hashable ConnectorProfileCredentials where
  hashWithSalt salt' ConnectorProfileCredentials' {..} =
    salt' `Prelude.hashWithSalt` veeva
      `Prelude.hashWithSalt` redshift
      `Prelude.hashWithSalt` trendmicro
      `Prelude.hashWithSalt` zendesk
      `Prelude.hashWithSalt` salesforce
      `Prelude.hashWithSalt` sAPOData
      `Prelude.hashWithSalt` googleAnalytics
      `Prelude.hashWithSalt` datadog
      `Prelude.hashWithSalt` amplitude
      `Prelude.hashWithSalt` inforNexus
      `Prelude.hashWithSalt` singular
      `Prelude.hashWithSalt` slack
      `Prelude.hashWithSalt` marketo
      `Prelude.hashWithSalt` dynatrace
      `Prelude.hashWithSalt` serviceNow
      `Prelude.hashWithSalt` honeycode
      `Prelude.hashWithSalt` snowflake

instance Prelude.NFData ConnectorProfileCredentials where
  rnf ConnectorProfileCredentials' {..} =
    Prelude.rnf snowflake
      `Prelude.seq` Prelude.rnf veeva
      `Prelude.seq` Prelude.rnf redshift
      `Prelude.seq` Prelude.rnf trendmicro
      `Prelude.seq` Prelude.rnf zendesk
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf sAPOData
      `Prelude.seq` Prelude.rnf googleAnalytics
      `Prelude.seq` Prelude.rnf datadog
      `Prelude.seq` Prelude.rnf amplitude
      `Prelude.seq` Prelude.rnf inforNexus
      `Prelude.seq` Prelude.rnf singular
      `Prelude.seq` Prelude.rnf slack
      `Prelude.seq` Prelude.rnf marketo
      `Prelude.seq` Prelude.rnf dynatrace
      `Prelude.seq` Prelude.rnf serviceNow
      `Prelude.seq` Prelude.rnf honeycode

instance Core.ToJSON ConnectorProfileCredentials where
  toJSON ConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Snowflake" Core..=) Prelude.<$> snowflake,
            ("Honeycode" Core..=) Prelude.<$> honeycode,
            ("ServiceNow" Core..=) Prelude.<$> serviceNow,
            ("Dynatrace" Core..=) Prelude.<$> dynatrace,
            ("Marketo" Core..=) Prelude.<$> marketo,
            ("Slack" Core..=) Prelude.<$> slack,
            ("Singular" Core..=) Prelude.<$> singular,
            ("InforNexus" Core..=) Prelude.<$> inforNexus,
            ("Amplitude" Core..=) Prelude.<$> amplitude,
            ("Datadog" Core..=) Prelude.<$> datadog,
            ("GoogleAnalytics" Core..=)
              Prelude.<$> googleAnalytics,
            ("SAPOData" Core..=) Prelude.<$> sAPOData,
            ("Salesforce" Core..=) Prelude.<$> salesforce,
            ("Zendesk" Core..=) Prelude.<$> zendesk,
            ("Trendmicro" Core..=) Prelude.<$> trendmicro,
            ("Redshift" Core..=) Prelude.<$> redshift,
            ("Veeva" Core..=) Prelude.<$> veeva
          ]
      )
