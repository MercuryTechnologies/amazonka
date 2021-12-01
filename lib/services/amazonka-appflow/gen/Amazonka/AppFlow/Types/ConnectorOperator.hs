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
-- Module      : Amazonka.AppFlow.Types.ConnectorOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorOperator where

import Amazonka.AppFlow.Types.AmplitudeConnectorOperator
import Amazonka.AppFlow.Types.DatadogConnectorOperator
import Amazonka.AppFlow.Types.DynatraceConnectorOperator
import Amazonka.AppFlow.Types.GoogleAnalyticsConnectorOperator
import Amazonka.AppFlow.Types.InforNexusConnectorOperator
import Amazonka.AppFlow.Types.MarketoConnectorOperator
import Amazonka.AppFlow.Types.S3ConnectorOperator
import Amazonka.AppFlow.Types.SAPODataConnectorOperator
import Amazonka.AppFlow.Types.SalesforceConnectorOperator
import Amazonka.AppFlow.Types.ServiceNowConnectorOperator
import Amazonka.AppFlow.Types.SingularConnectorOperator
import Amazonka.AppFlow.Types.SlackConnectorOperator
import Amazonka.AppFlow.Types.TrendmicroConnectorOperator
import Amazonka.AppFlow.Types.VeevaConnectorOperator
import Amazonka.AppFlow.Types.ZendeskConnectorOperator
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The operation to be performed on the provided source fields.
--
-- /See:/ 'newConnectorOperator' smart constructor.
data ConnectorOperator = ConnectorOperator'
  { -- | The operation to be performed on the provided ServiceNow source fields.
    serviceNow :: Prelude.Maybe ServiceNowConnectorOperator,
    -- | The operation to be performed on the provided Dynatrace source fields.
    dynatrace :: Prelude.Maybe DynatraceConnectorOperator,
    -- | The operation to be performed on the provided Marketo source fields.
    marketo :: Prelude.Maybe MarketoConnectorOperator,
    -- | The operation to be performed on the provided Slack source fields.
    slack :: Prelude.Maybe SlackConnectorOperator,
    -- | The operation to be performed on the provided Singular source fields.
    singular :: Prelude.Maybe SingularConnectorOperator,
    -- | The operation to be performed on the provided Infor Nexus source fields.
    inforNexus :: Prelude.Maybe InforNexusConnectorOperator,
    -- | The operation to be performed on the provided Amplitude source fields.
    amplitude :: Prelude.Maybe AmplitudeConnectorOperator,
    -- | The operation to be performed on the provided Datadog source fields.
    datadog :: Prelude.Maybe DatadogConnectorOperator,
    -- | The operation to be performed on the provided Google Analytics source
    -- fields.
    googleAnalytics :: Prelude.Maybe GoogleAnalyticsConnectorOperator,
    -- | The operation to be performed on the provided SAPOData source fields.
    sAPOData :: Prelude.Maybe SAPODataConnectorOperator,
    -- | The operation to be performed on the provided Salesforce source fields.
    salesforce :: Prelude.Maybe SalesforceConnectorOperator,
    -- | The operation to be performed on the provided Zendesk source fields.
    zendesk :: Prelude.Maybe ZendeskConnectorOperator,
    -- | The operation to be performed on the provided Amazon S3 source fields.
    s3 :: Prelude.Maybe S3ConnectorOperator,
    -- | The operation to be performed on the provided Trend Micro source fields.
    trendmicro :: Prelude.Maybe TrendmicroConnectorOperator,
    -- | The operation to be performed on the provided Veeva source fields.
    veeva :: Prelude.Maybe VeevaConnectorOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorOperator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNow', 'connectorOperator_serviceNow' - The operation to be performed on the provided ServiceNow source fields.
--
-- 'dynatrace', 'connectorOperator_dynatrace' - The operation to be performed on the provided Dynatrace source fields.
--
-- 'marketo', 'connectorOperator_marketo' - The operation to be performed on the provided Marketo source fields.
--
-- 'slack', 'connectorOperator_slack' - The operation to be performed on the provided Slack source fields.
--
-- 'singular', 'connectorOperator_singular' - The operation to be performed on the provided Singular source fields.
--
-- 'inforNexus', 'connectorOperator_inforNexus' - The operation to be performed on the provided Infor Nexus source fields.
--
-- 'amplitude', 'connectorOperator_amplitude' - The operation to be performed on the provided Amplitude source fields.
--
-- 'datadog', 'connectorOperator_datadog' - The operation to be performed on the provided Datadog source fields.
--
-- 'googleAnalytics', 'connectorOperator_googleAnalytics' - The operation to be performed on the provided Google Analytics source
-- fields.
--
-- 'sAPOData', 'connectorOperator_sAPOData' - The operation to be performed on the provided SAPOData source fields.
--
-- 'salesforce', 'connectorOperator_salesforce' - The operation to be performed on the provided Salesforce source fields.
--
-- 'zendesk', 'connectorOperator_zendesk' - The operation to be performed on the provided Zendesk source fields.
--
-- 's3', 'connectorOperator_s3' - The operation to be performed on the provided Amazon S3 source fields.
--
-- 'trendmicro', 'connectorOperator_trendmicro' - The operation to be performed on the provided Trend Micro source fields.
--
-- 'veeva', 'connectorOperator_veeva' - The operation to be performed on the provided Veeva source fields.
newConnectorOperator ::
  ConnectorOperator
newConnectorOperator =
  ConnectorOperator'
    { serviceNow = Prelude.Nothing,
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
      s3 = Prelude.Nothing,
      trendmicro = Prelude.Nothing,
      veeva = Prelude.Nothing
    }

-- | The operation to be performed on the provided ServiceNow source fields.
connectorOperator_serviceNow :: Lens.Lens' ConnectorOperator (Prelude.Maybe ServiceNowConnectorOperator)
connectorOperator_serviceNow = Lens.lens (\ConnectorOperator' {serviceNow} -> serviceNow) (\s@ConnectorOperator' {} a -> s {serviceNow = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Dynatrace source fields.
connectorOperator_dynatrace :: Lens.Lens' ConnectorOperator (Prelude.Maybe DynatraceConnectorOperator)
connectorOperator_dynatrace = Lens.lens (\ConnectorOperator' {dynatrace} -> dynatrace) (\s@ConnectorOperator' {} a -> s {dynatrace = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Marketo source fields.
connectorOperator_marketo :: Lens.Lens' ConnectorOperator (Prelude.Maybe MarketoConnectorOperator)
connectorOperator_marketo = Lens.lens (\ConnectorOperator' {marketo} -> marketo) (\s@ConnectorOperator' {} a -> s {marketo = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Slack source fields.
connectorOperator_slack :: Lens.Lens' ConnectorOperator (Prelude.Maybe SlackConnectorOperator)
connectorOperator_slack = Lens.lens (\ConnectorOperator' {slack} -> slack) (\s@ConnectorOperator' {} a -> s {slack = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Singular source fields.
connectorOperator_singular :: Lens.Lens' ConnectorOperator (Prelude.Maybe SingularConnectorOperator)
connectorOperator_singular = Lens.lens (\ConnectorOperator' {singular} -> singular) (\s@ConnectorOperator' {} a -> s {singular = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Infor Nexus source fields.
connectorOperator_inforNexus :: Lens.Lens' ConnectorOperator (Prelude.Maybe InforNexusConnectorOperator)
connectorOperator_inforNexus = Lens.lens (\ConnectorOperator' {inforNexus} -> inforNexus) (\s@ConnectorOperator' {} a -> s {inforNexus = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Amplitude source fields.
connectorOperator_amplitude :: Lens.Lens' ConnectorOperator (Prelude.Maybe AmplitudeConnectorOperator)
connectorOperator_amplitude = Lens.lens (\ConnectorOperator' {amplitude} -> amplitude) (\s@ConnectorOperator' {} a -> s {amplitude = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Datadog source fields.
connectorOperator_datadog :: Lens.Lens' ConnectorOperator (Prelude.Maybe DatadogConnectorOperator)
connectorOperator_datadog = Lens.lens (\ConnectorOperator' {datadog} -> datadog) (\s@ConnectorOperator' {} a -> s {datadog = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Google Analytics source
-- fields.
connectorOperator_googleAnalytics :: Lens.Lens' ConnectorOperator (Prelude.Maybe GoogleAnalyticsConnectorOperator)
connectorOperator_googleAnalytics = Lens.lens (\ConnectorOperator' {googleAnalytics} -> googleAnalytics) (\s@ConnectorOperator' {} a -> s {googleAnalytics = a} :: ConnectorOperator)

-- | The operation to be performed on the provided SAPOData source fields.
connectorOperator_sAPOData :: Lens.Lens' ConnectorOperator (Prelude.Maybe SAPODataConnectorOperator)
connectorOperator_sAPOData = Lens.lens (\ConnectorOperator' {sAPOData} -> sAPOData) (\s@ConnectorOperator' {} a -> s {sAPOData = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Salesforce source fields.
connectorOperator_salesforce :: Lens.Lens' ConnectorOperator (Prelude.Maybe SalesforceConnectorOperator)
connectorOperator_salesforce = Lens.lens (\ConnectorOperator' {salesforce} -> salesforce) (\s@ConnectorOperator' {} a -> s {salesforce = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Zendesk source fields.
connectorOperator_zendesk :: Lens.Lens' ConnectorOperator (Prelude.Maybe ZendeskConnectorOperator)
connectorOperator_zendesk = Lens.lens (\ConnectorOperator' {zendesk} -> zendesk) (\s@ConnectorOperator' {} a -> s {zendesk = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Amazon S3 source fields.
connectorOperator_s3 :: Lens.Lens' ConnectorOperator (Prelude.Maybe S3ConnectorOperator)
connectorOperator_s3 = Lens.lens (\ConnectorOperator' {s3} -> s3) (\s@ConnectorOperator' {} a -> s {s3 = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Trend Micro source fields.
connectorOperator_trendmicro :: Lens.Lens' ConnectorOperator (Prelude.Maybe TrendmicroConnectorOperator)
connectorOperator_trendmicro = Lens.lens (\ConnectorOperator' {trendmicro} -> trendmicro) (\s@ConnectorOperator' {} a -> s {trendmicro = a} :: ConnectorOperator)

-- | The operation to be performed on the provided Veeva source fields.
connectorOperator_veeva :: Lens.Lens' ConnectorOperator (Prelude.Maybe VeevaConnectorOperator)
connectorOperator_veeva = Lens.lens (\ConnectorOperator' {veeva} -> veeva) (\s@ConnectorOperator' {} a -> s {veeva = a} :: ConnectorOperator)

instance Core.FromJSON ConnectorOperator where
  parseJSON =
    Core.withObject
      "ConnectorOperator"
      ( \x ->
          ConnectorOperator'
            Prelude.<$> (x Core..:? "ServiceNow")
            Prelude.<*> (x Core..:? "Dynatrace")
            Prelude.<*> (x Core..:? "Marketo")
            Prelude.<*> (x Core..:? "Slack")
            Prelude.<*> (x Core..:? "Singular")
            Prelude.<*> (x Core..:? "InforNexus")
            Prelude.<*> (x Core..:? "Amplitude")
            Prelude.<*> (x Core..:? "Datadog")
            Prelude.<*> (x Core..:? "GoogleAnalytics")
            Prelude.<*> (x Core..:? "SAPOData")
            Prelude.<*> (x Core..:? "Salesforce")
            Prelude.<*> (x Core..:? "Zendesk")
            Prelude.<*> (x Core..:? "S3")
            Prelude.<*> (x Core..:? "Trendmicro")
            Prelude.<*> (x Core..:? "Veeva")
      )

instance Prelude.Hashable ConnectorOperator where
  hashWithSalt salt' ConnectorOperator' {..} =
    salt' `Prelude.hashWithSalt` veeva
      `Prelude.hashWithSalt` trendmicro
      `Prelude.hashWithSalt` s3
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

instance Prelude.NFData ConnectorOperator where
  rnf ConnectorOperator' {..} =
    Prelude.rnf serviceNow
      `Prelude.seq` Prelude.rnf veeva
      `Prelude.seq` Prelude.rnf trendmicro
      `Prelude.seq` Prelude.rnf s3
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

instance Core.ToJSON ConnectorOperator where
  toJSON ConnectorOperator' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServiceNow" Core..=) Prelude.<$> serviceNow,
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
            ("S3" Core..=) Prelude.<$> s3,
            ("Trendmicro" Core..=) Prelude.<$> trendmicro,
            ("Veeva" Core..=) Prelude.<$> veeva
          ]
      )
