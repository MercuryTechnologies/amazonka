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
-- Module      : Amazonka.ElastiCache.Types.ServiceUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.ServiceUpdate where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.ServiceUpdateSeverity
import Amazonka.ElastiCache.Types.ServiceUpdateStatus
import Amazonka.ElastiCache.Types.ServiceUpdateType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An update that you can apply to your Redis clusters.
--
-- /See:/ 'newServiceUpdate' smart constructor.
data ServiceUpdate = ServiceUpdate'
  { -- | The Elasticache engine version to which the update applies. Either Redis
    -- or Memcached engine version
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Reflects the nature of the service update
    serviceUpdateType :: Prelude.Maybe ServiceUpdateType,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | The Elasticache engine to which the update applies. Either Redis or
    -- Memcached
    engine :: Prelude.Maybe Prelude.Text,
    -- | The date when the service update is initially available
    serviceUpdateReleaseDate :: Prelude.Maybe Core.ISO8601,
    -- | Indicates whether the service update will be automatically applied once
    -- the recommended apply-by date has expired.
    autoUpdateAfterRecommendedApplyByDate :: Prelude.Maybe Prelude.Bool,
    -- | The severity of the service update
    serviceUpdateSeverity :: Prelude.Maybe ServiceUpdateSeverity,
    -- | The date after which the service update is no longer available
    serviceUpdateEndDate :: Prelude.Maybe Core.ISO8601,
    -- | Provides details of the service update
    serviceUpdateDescription :: Prelude.Maybe Prelude.Text,
    -- | The recommendend date to apply the service update in order to ensure
    -- compliance. For information on compliance, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
    serviceUpdateRecommendedApplyByDate :: Prelude.Maybe Core.ISO8601,
    -- | The status of the service update
    serviceUpdateStatus :: Prelude.Maybe ServiceUpdateStatus,
    -- | The estimated length of time the service update will take
    estimatedUpdateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'serviceUpdate_engineVersion' - The Elasticache engine version to which the update applies. Either Redis
-- or Memcached engine version
--
-- 'serviceUpdateType', 'serviceUpdate_serviceUpdateType' - Reflects the nature of the service update
--
-- 'serviceUpdateName', 'serviceUpdate_serviceUpdateName' - The unique ID of the service update
--
-- 'engine', 'serviceUpdate_engine' - The Elasticache engine to which the update applies. Either Redis or
-- Memcached
--
-- 'serviceUpdateReleaseDate', 'serviceUpdate_serviceUpdateReleaseDate' - The date when the service update is initially available
--
-- 'autoUpdateAfterRecommendedApplyByDate', 'serviceUpdate_autoUpdateAfterRecommendedApplyByDate' - Indicates whether the service update will be automatically applied once
-- the recommended apply-by date has expired.
--
-- 'serviceUpdateSeverity', 'serviceUpdate_serviceUpdateSeverity' - The severity of the service update
--
-- 'serviceUpdateEndDate', 'serviceUpdate_serviceUpdateEndDate' - The date after which the service update is no longer available
--
-- 'serviceUpdateDescription', 'serviceUpdate_serviceUpdateDescription' - Provides details of the service update
--
-- 'serviceUpdateRecommendedApplyByDate', 'serviceUpdate_serviceUpdateRecommendedApplyByDate' - The recommendend date to apply the service update in order to ensure
-- compliance. For information on compliance, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
--
-- 'serviceUpdateStatus', 'serviceUpdate_serviceUpdateStatus' - The status of the service update
--
-- 'estimatedUpdateTime', 'serviceUpdate_estimatedUpdateTime' - The estimated length of time the service update will take
newServiceUpdate ::
  ServiceUpdate
newServiceUpdate =
  ServiceUpdate'
    { engineVersion = Prelude.Nothing,
      serviceUpdateType = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      engine = Prelude.Nothing,
      serviceUpdateReleaseDate = Prelude.Nothing,
      autoUpdateAfterRecommendedApplyByDate =
        Prelude.Nothing,
      serviceUpdateSeverity = Prelude.Nothing,
      serviceUpdateEndDate = Prelude.Nothing,
      serviceUpdateDescription = Prelude.Nothing,
      serviceUpdateRecommendedApplyByDate =
        Prelude.Nothing,
      serviceUpdateStatus = Prelude.Nothing,
      estimatedUpdateTime = Prelude.Nothing
    }

-- | The Elasticache engine version to which the update applies. Either Redis
-- or Memcached engine version
serviceUpdate_engineVersion :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_engineVersion = Lens.lens (\ServiceUpdate' {engineVersion} -> engineVersion) (\s@ServiceUpdate' {} a -> s {engineVersion = a} :: ServiceUpdate)

-- | Reflects the nature of the service update
serviceUpdate_serviceUpdateType :: Lens.Lens' ServiceUpdate (Prelude.Maybe ServiceUpdateType)
serviceUpdate_serviceUpdateType = Lens.lens (\ServiceUpdate' {serviceUpdateType} -> serviceUpdateType) (\s@ServiceUpdate' {} a -> s {serviceUpdateType = a} :: ServiceUpdate)

-- | The unique ID of the service update
serviceUpdate_serviceUpdateName :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_serviceUpdateName = Lens.lens (\ServiceUpdate' {serviceUpdateName} -> serviceUpdateName) (\s@ServiceUpdate' {} a -> s {serviceUpdateName = a} :: ServiceUpdate)

-- | The Elasticache engine to which the update applies. Either Redis or
-- Memcached
serviceUpdate_engine :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_engine = Lens.lens (\ServiceUpdate' {engine} -> engine) (\s@ServiceUpdate' {} a -> s {engine = a} :: ServiceUpdate)

-- | The date when the service update is initially available
serviceUpdate_serviceUpdateReleaseDate :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.UTCTime)
serviceUpdate_serviceUpdateReleaseDate = Lens.lens (\ServiceUpdate' {serviceUpdateReleaseDate} -> serviceUpdateReleaseDate) (\s@ServiceUpdate' {} a -> s {serviceUpdateReleaseDate = a} :: ServiceUpdate) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the service update will be automatically applied once
-- the recommended apply-by date has expired.
serviceUpdate_autoUpdateAfterRecommendedApplyByDate :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Bool)
serviceUpdate_autoUpdateAfterRecommendedApplyByDate = Lens.lens (\ServiceUpdate' {autoUpdateAfterRecommendedApplyByDate} -> autoUpdateAfterRecommendedApplyByDate) (\s@ServiceUpdate' {} a -> s {autoUpdateAfterRecommendedApplyByDate = a} :: ServiceUpdate)

-- | The severity of the service update
serviceUpdate_serviceUpdateSeverity :: Lens.Lens' ServiceUpdate (Prelude.Maybe ServiceUpdateSeverity)
serviceUpdate_serviceUpdateSeverity = Lens.lens (\ServiceUpdate' {serviceUpdateSeverity} -> serviceUpdateSeverity) (\s@ServiceUpdate' {} a -> s {serviceUpdateSeverity = a} :: ServiceUpdate)

-- | The date after which the service update is no longer available
serviceUpdate_serviceUpdateEndDate :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.UTCTime)
serviceUpdate_serviceUpdateEndDate = Lens.lens (\ServiceUpdate' {serviceUpdateEndDate} -> serviceUpdateEndDate) (\s@ServiceUpdate' {} a -> s {serviceUpdateEndDate = a} :: ServiceUpdate) Prelude.. Lens.mapping Core._Time

-- | Provides details of the service update
serviceUpdate_serviceUpdateDescription :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_serviceUpdateDescription = Lens.lens (\ServiceUpdate' {serviceUpdateDescription} -> serviceUpdateDescription) (\s@ServiceUpdate' {} a -> s {serviceUpdateDescription = a} :: ServiceUpdate)

-- | The recommendend date to apply the service update in order to ensure
-- compliance. For information on compliance, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/elasticache-compliance.html#elasticache-compliance-self-service Self-Service Security Updates for Compliance>.
serviceUpdate_serviceUpdateRecommendedApplyByDate :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.UTCTime)
serviceUpdate_serviceUpdateRecommendedApplyByDate = Lens.lens (\ServiceUpdate' {serviceUpdateRecommendedApplyByDate} -> serviceUpdateRecommendedApplyByDate) (\s@ServiceUpdate' {} a -> s {serviceUpdateRecommendedApplyByDate = a} :: ServiceUpdate) Prelude.. Lens.mapping Core._Time

-- | The status of the service update
serviceUpdate_serviceUpdateStatus :: Lens.Lens' ServiceUpdate (Prelude.Maybe ServiceUpdateStatus)
serviceUpdate_serviceUpdateStatus = Lens.lens (\ServiceUpdate' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@ServiceUpdate' {} a -> s {serviceUpdateStatus = a} :: ServiceUpdate)

-- | The estimated length of time the service update will take
serviceUpdate_estimatedUpdateTime :: Lens.Lens' ServiceUpdate (Prelude.Maybe Prelude.Text)
serviceUpdate_estimatedUpdateTime = Lens.lens (\ServiceUpdate' {estimatedUpdateTime} -> estimatedUpdateTime) (\s@ServiceUpdate' {} a -> s {estimatedUpdateTime = a} :: ServiceUpdate)

instance Core.FromXML ServiceUpdate where
  parseXML x =
    ServiceUpdate'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "ServiceUpdateType")
      Prelude.<*> (x Core..@? "ServiceUpdateName")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "ServiceUpdateReleaseDate")
      Prelude.<*> (x Core..@? "AutoUpdateAfterRecommendedApplyByDate")
      Prelude.<*> (x Core..@? "ServiceUpdateSeverity")
      Prelude.<*> (x Core..@? "ServiceUpdateEndDate")
      Prelude.<*> (x Core..@? "ServiceUpdateDescription")
      Prelude.<*> (x Core..@? "ServiceUpdateRecommendedApplyByDate")
      Prelude.<*> (x Core..@? "ServiceUpdateStatus")
      Prelude.<*> (x Core..@? "EstimatedUpdateTime")

instance Prelude.Hashable ServiceUpdate where
  hashWithSalt salt' ServiceUpdate' {..} =
    salt' `Prelude.hashWithSalt` estimatedUpdateTime
      `Prelude.hashWithSalt` serviceUpdateStatus
      `Prelude.hashWithSalt` serviceUpdateRecommendedApplyByDate
      `Prelude.hashWithSalt` serviceUpdateDescription
      `Prelude.hashWithSalt` serviceUpdateEndDate
      `Prelude.hashWithSalt` serviceUpdateSeverity
      `Prelude.hashWithSalt` autoUpdateAfterRecommendedApplyByDate
      `Prelude.hashWithSalt` serviceUpdateReleaseDate
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` serviceUpdateName
      `Prelude.hashWithSalt` serviceUpdateType
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData ServiceUpdate where
  rnf ServiceUpdate' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf estimatedUpdateTime
      `Prelude.seq` Prelude.rnf serviceUpdateStatus
      `Prelude.seq` Prelude.rnf serviceUpdateRecommendedApplyByDate
      `Prelude.seq` Prelude.rnf serviceUpdateDescription
      `Prelude.seq` Prelude.rnf serviceUpdateEndDate
      `Prelude.seq` Prelude.rnf serviceUpdateSeverity
      `Prelude.seq` Prelude.rnf autoUpdateAfterRecommendedApplyByDate
      `Prelude.seq` Prelude.rnf serviceUpdateReleaseDate
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf serviceUpdateName
      `Prelude.seq` Prelude.rnf serviceUpdateType
