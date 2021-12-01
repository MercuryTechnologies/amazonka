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
-- Module      : Amazonka.AppFlow.Types.DestinationConnectorProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DestinationConnectorProperties where

import Amazonka.AppFlow.Types.CustomerProfilesDestinationProperties
import Amazonka.AppFlow.Types.EventBridgeDestinationProperties
import Amazonka.AppFlow.Types.HoneycodeDestinationProperties
import Amazonka.AppFlow.Types.LookoutMetricsDestinationProperties
import Amazonka.AppFlow.Types.RedshiftDestinationProperties
import Amazonka.AppFlow.Types.S3DestinationProperties
import Amazonka.AppFlow.Types.SalesforceDestinationProperties
import Amazonka.AppFlow.Types.SnowflakeDestinationProperties
import Amazonka.AppFlow.Types.UpsolverDestinationProperties
import Amazonka.AppFlow.Types.ZendeskDestinationProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | This stores the information that is required to query a particular
-- connector.
--
-- /See:/ 'newDestinationConnectorProperties' smart constructor.
data DestinationConnectorProperties = DestinationConnectorProperties'
  { -- | The properties required to query Upsolver.
    upsolver :: Prelude.Maybe UpsolverDestinationProperties,
    -- | The properties required to query Snowflake.
    snowflake :: Prelude.Maybe SnowflakeDestinationProperties,
    -- | The properties required to query Amazon Honeycode.
    honeycode :: Prelude.Maybe HoneycodeDestinationProperties,
    -- | The properties required to query Amazon Lookout for Metrics.
    lookoutMetrics :: Prelude.Maybe LookoutMetricsDestinationProperties,
    -- | The properties required to query Amazon Connect Customer Profiles.
    customerProfiles :: Prelude.Maybe CustomerProfilesDestinationProperties,
    -- | The properties required to query Salesforce.
    salesforce :: Prelude.Maybe SalesforceDestinationProperties,
    -- | The properties required to query Zendesk.
    zendesk :: Prelude.Maybe ZendeskDestinationProperties,
    -- | The properties required to query Amazon S3.
    s3 :: Prelude.Maybe S3DestinationProperties,
    -- | The properties required to query Amazon EventBridge.
    eventBridge :: Prelude.Maybe EventBridgeDestinationProperties,
    -- | The properties required to query Amazon Redshift.
    redshift :: Prelude.Maybe RedshiftDestinationProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationConnectorProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upsolver', 'destinationConnectorProperties_upsolver' - The properties required to query Upsolver.
--
-- 'snowflake', 'destinationConnectorProperties_snowflake' - The properties required to query Snowflake.
--
-- 'honeycode', 'destinationConnectorProperties_honeycode' - The properties required to query Amazon Honeycode.
--
-- 'lookoutMetrics', 'destinationConnectorProperties_lookoutMetrics' - The properties required to query Amazon Lookout for Metrics.
--
-- 'customerProfiles', 'destinationConnectorProperties_customerProfiles' - The properties required to query Amazon Connect Customer Profiles.
--
-- 'salesforce', 'destinationConnectorProperties_salesforce' - The properties required to query Salesforce.
--
-- 'zendesk', 'destinationConnectorProperties_zendesk' - The properties required to query Zendesk.
--
-- 's3', 'destinationConnectorProperties_s3' - The properties required to query Amazon S3.
--
-- 'eventBridge', 'destinationConnectorProperties_eventBridge' - The properties required to query Amazon EventBridge.
--
-- 'redshift', 'destinationConnectorProperties_redshift' - The properties required to query Amazon Redshift.
newDestinationConnectorProperties ::
  DestinationConnectorProperties
newDestinationConnectorProperties =
  DestinationConnectorProperties'
    { upsolver =
        Prelude.Nothing,
      snowflake = Prelude.Nothing,
      honeycode = Prelude.Nothing,
      lookoutMetrics = Prelude.Nothing,
      customerProfiles = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      zendesk = Prelude.Nothing,
      s3 = Prelude.Nothing,
      eventBridge = Prelude.Nothing,
      redshift = Prelude.Nothing
    }

-- | The properties required to query Upsolver.
destinationConnectorProperties_upsolver :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe UpsolverDestinationProperties)
destinationConnectorProperties_upsolver = Lens.lens (\DestinationConnectorProperties' {upsolver} -> upsolver) (\s@DestinationConnectorProperties' {} a -> s {upsolver = a} :: DestinationConnectorProperties)

-- | The properties required to query Snowflake.
destinationConnectorProperties_snowflake :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe SnowflakeDestinationProperties)
destinationConnectorProperties_snowflake = Lens.lens (\DestinationConnectorProperties' {snowflake} -> snowflake) (\s@DestinationConnectorProperties' {} a -> s {snowflake = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon Honeycode.
destinationConnectorProperties_honeycode :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe HoneycodeDestinationProperties)
destinationConnectorProperties_honeycode = Lens.lens (\DestinationConnectorProperties' {honeycode} -> honeycode) (\s@DestinationConnectorProperties' {} a -> s {honeycode = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon Lookout for Metrics.
destinationConnectorProperties_lookoutMetrics :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe LookoutMetricsDestinationProperties)
destinationConnectorProperties_lookoutMetrics = Lens.lens (\DestinationConnectorProperties' {lookoutMetrics} -> lookoutMetrics) (\s@DestinationConnectorProperties' {} a -> s {lookoutMetrics = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon Connect Customer Profiles.
destinationConnectorProperties_customerProfiles :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe CustomerProfilesDestinationProperties)
destinationConnectorProperties_customerProfiles = Lens.lens (\DestinationConnectorProperties' {customerProfiles} -> customerProfiles) (\s@DestinationConnectorProperties' {} a -> s {customerProfiles = a} :: DestinationConnectorProperties)

-- | The properties required to query Salesforce.
destinationConnectorProperties_salesforce :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe SalesforceDestinationProperties)
destinationConnectorProperties_salesforce = Lens.lens (\DestinationConnectorProperties' {salesforce} -> salesforce) (\s@DestinationConnectorProperties' {} a -> s {salesforce = a} :: DestinationConnectorProperties)

-- | The properties required to query Zendesk.
destinationConnectorProperties_zendesk :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe ZendeskDestinationProperties)
destinationConnectorProperties_zendesk = Lens.lens (\DestinationConnectorProperties' {zendesk} -> zendesk) (\s@DestinationConnectorProperties' {} a -> s {zendesk = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon S3.
destinationConnectorProperties_s3 :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe S3DestinationProperties)
destinationConnectorProperties_s3 = Lens.lens (\DestinationConnectorProperties' {s3} -> s3) (\s@DestinationConnectorProperties' {} a -> s {s3 = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon EventBridge.
destinationConnectorProperties_eventBridge :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe EventBridgeDestinationProperties)
destinationConnectorProperties_eventBridge = Lens.lens (\DestinationConnectorProperties' {eventBridge} -> eventBridge) (\s@DestinationConnectorProperties' {} a -> s {eventBridge = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon Redshift.
destinationConnectorProperties_redshift :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe RedshiftDestinationProperties)
destinationConnectorProperties_redshift = Lens.lens (\DestinationConnectorProperties' {redshift} -> redshift) (\s@DestinationConnectorProperties' {} a -> s {redshift = a} :: DestinationConnectorProperties)

instance Core.FromJSON DestinationConnectorProperties where
  parseJSON =
    Core.withObject
      "DestinationConnectorProperties"
      ( \x ->
          DestinationConnectorProperties'
            Prelude.<$> (x Core..:? "Upsolver")
            Prelude.<*> (x Core..:? "Snowflake")
            Prelude.<*> (x Core..:? "Honeycode")
            Prelude.<*> (x Core..:? "LookoutMetrics")
            Prelude.<*> (x Core..:? "CustomerProfiles")
            Prelude.<*> (x Core..:? "Salesforce")
            Prelude.<*> (x Core..:? "Zendesk")
            Prelude.<*> (x Core..:? "S3")
            Prelude.<*> (x Core..:? "EventBridge")
            Prelude.<*> (x Core..:? "Redshift")
      )

instance
  Prelude.Hashable
    DestinationConnectorProperties
  where
  hashWithSalt
    salt'
    DestinationConnectorProperties' {..} =
      salt' `Prelude.hashWithSalt` redshift
        `Prelude.hashWithSalt` eventBridge
        `Prelude.hashWithSalt` s3
        `Prelude.hashWithSalt` zendesk
        `Prelude.hashWithSalt` salesforce
        `Prelude.hashWithSalt` customerProfiles
        `Prelude.hashWithSalt` lookoutMetrics
        `Prelude.hashWithSalt` honeycode
        `Prelude.hashWithSalt` snowflake
        `Prelude.hashWithSalt` upsolver

instance
  Prelude.NFData
    DestinationConnectorProperties
  where
  rnf DestinationConnectorProperties' {..} =
    Prelude.rnf upsolver
      `Prelude.seq` Prelude.rnf redshift
      `Prelude.seq` Prelude.rnf eventBridge
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf zendesk
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf customerProfiles
      `Prelude.seq` Prelude.rnf lookoutMetrics
      `Prelude.seq` Prelude.rnf honeycode
      `Prelude.seq` Prelude.rnf snowflake

instance Core.ToJSON DestinationConnectorProperties where
  toJSON DestinationConnectorProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Upsolver" Core..=) Prelude.<$> upsolver,
            ("Snowflake" Core..=) Prelude.<$> snowflake,
            ("Honeycode" Core..=) Prelude.<$> honeycode,
            ("LookoutMetrics" Core..=)
              Prelude.<$> lookoutMetrics,
            ("CustomerProfiles" Core..=)
              Prelude.<$> customerProfiles,
            ("Salesforce" Core..=) Prelude.<$> salesforce,
            ("Zendesk" Core..=) Prelude.<$> zendesk,
            ("S3" Core..=) Prelude.<$> s3,
            ("EventBridge" Core..=) Prelude.<$> eventBridge,
            ("Redshift" Core..=) Prelude.<$> redshift
          ]
      )
