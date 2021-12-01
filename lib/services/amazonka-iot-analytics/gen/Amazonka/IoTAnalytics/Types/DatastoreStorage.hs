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
-- Module      : Amazonka.IoTAnalytics.Types.DatastoreStorage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatastoreStorage where

import qualified Amazonka.Core as Core
import Amazonka.IoTAnalytics.Types.CustomerManagedDatastoreS3Storage
import Amazonka.IoTAnalytics.Types.DatastoreIotSiteWiseMultiLayerStorage
import Amazonka.IoTAnalytics.Types.ServiceManagedDatastoreS3Storage
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Where data in a data store is stored.. You can choose @serviceManagedS3@
-- storage, @customerManagedS3@ storage, or @iotSiteWiseMultiLayerStorage@
-- storage. The default is @serviceManagedS3@. You can\'t change the choice
-- of Amazon S3 storage after your data store is created.
--
-- /See:/ 'newDatastoreStorage' smart constructor.
data DatastoreStorage = DatastoreStorage'
  { -- | Used to store data in an Amazon S3 bucket managed by IoT Analytics. You
    -- can\'t change the choice of Amazon S3 storage after your data store is
    -- created.
    serviceManagedS3 :: Prelude.Maybe ServiceManagedDatastoreS3Storage,
    -- | S3-customer-managed; When you choose customer-managed storage, the
    -- @retentionPeriod@ parameter is ignored. You can\'t change the choice of
    -- Amazon S3 storage after your data store is created.
    customerManagedS3 :: Prelude.Maybe CustomerManagedDatastoreS3Storage,
    -- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
    -- manage. You can\'t change the choice of Amazon S3 storage after your
    -- data store is created.
    iotSiteWiseMultiLayerStorage :: Prelude.Maybe DatastoreIotSiteWiseMultiLayerStorage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatastoreStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceManagedS3', 'datastoreStorage_serviceManagedS3' - Used to store data in an Amazon S3 bucket managed by IoT Analytics. You
-- can\'t change the choice of Amazon S3 storage after your data store is
-- created.
--
-- 'customerManagedS3', 'datastoreStorage_customerManagedS3' - S3-customer-managed; When you choose customer-managed storage, the
-- @retentionPeriod@ parameter is ignored. You can\'t change the choice of
-- Amazon S3 storage after your data store is created.
--
-- 'iotSiteWiseMultiLayerStorage', 'datastoreStorage_iotSiteWiseMultiLayerStorage' - Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage. You can\'t change the choice of Amazon S3 storage after your
-- data store is created.
newDatastoreStorage ::
  DatastoreStorage
newDatastoreStorage =
  DatastoreStorage'
    { serviceManagedS3 =
        Prelude.Nothing,
      customerManagedS3 = Prelude.Nothing,
      iotSiteWiseMultiLayerStorage = Prelude.Nothing
    }

-- | Used to store data in an Amazon S3 bucket managed by IoT Analytics. You
-- can\'t change the choice of Amazon S3 storage after your data store is
-- created.
datastoreStorage_serviceManagedS3 :: Lens.Lens' DatastoreStorage (Prelude.Maybe ServiceManagedDatastoreS3Storage)
datastoreStorage_serviceManagedS3 = Lens.lens (\DatastoreStorage' {serviceManagedS3} -> serviceManagedS3) (\s@DatastoreStorage' {} a -> s {serviceManagedS3 = a} :: DatastoreStorage)

-- | S3-customer-managed; When you choose customer-managed storage, the
-- @retentionPeriod@ parameter is ignored. You can\'t change the choice of
-- Amazon S3 storage after your data store is created.
datastoreStorage_customerManagedS3 :: Lens.Lens' DatastoreStorage (Prelude.Maybe CustomerManagedDatastoreS3Storage)
datastoreStorage_customerManagedS3 = Lens.lens (\DatastoreStorage' {customerManagedS3} -> customerManagedS3) (\s@DatastoreStorage' {} a -> s {customerManagedS3 = a} :: DatastoreStorage)

-- | Used to store data used by IoT SiteWise in an Amazon S3 bucket that you
-- manage. You can\'t change the choice of Amazon S3 storage after your
-- data store is created.
datastoreStorage_iotSiteWiseMultiLayerStorage :: Lens.Lens' DatastoreStorage (Prelude.Maybe DatastoreIotSiteWiseMultiLayerStorage)
datastoreStorage_iotSiteWiseMultiLayerStorage = Lens.lens (\DatastoreStorage' {iotSiteWiseMultiLayerStorage} -> iotSiteWiseMultiLayerStorage) (\s@DatastoreStorage' {} a -> s {iotSiteWiseMultiLayerStorage = a} :: DatastoreStorage)

instance Core.FromJSON DatastoreStorage where
  parseJSON =
    Core.withObject
      "DatastoreStorage"
      ( \x ->
          DatastoreStorage'
            Prelude.<$> (x Core..:? "serviceManagedS3")
            Prelude.<*> (x Core..:? "customerManagedS3")
            Prelude.<*> (x Core..:? "iotSiteWiseMultiLayerStorage")
      )

instance Prelude.Hashable DatastoreStorage where
  hashWithSalt salt' DatastoreStorage' {..} =
    salt'
      `Prelude.hashWithSalt` iotSiteWiseMultiLayerStorage
      `Prelude.hashWithSalt` customerManagedS3
      `Prelude.hashWithSalt` serviceManagedS3

instance Prelude.NFData DatastoreStorage where
  rnf DatastoreStorage' {..} =
    Prelude.rnf serviceManagedS3
      `Prelude.seq` Prelude.rnf iotSiteWiseMultiLayerStorage
      `Prelude.seq` Prelude.rnf customerManagedS3

instance Core.ToJSON DatastoreStorage where
  toJSON DatastoreStorage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("serviceManagedS3" Core..=)
              Prelude.<$> serviceManagedS3,
            ("customerManagedS3" Core..=)
              Prelude.<$> customerManagedS3,
            ("iotSiteWiseMultiLayerStorage" Core..=)
              Prelude.<$> iotSiteWiseMultiLayerStorage
          ]
      )
