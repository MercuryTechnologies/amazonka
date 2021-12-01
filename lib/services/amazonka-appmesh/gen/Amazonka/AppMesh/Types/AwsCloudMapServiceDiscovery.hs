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
-- Module      : Amazonka.AppMesh.Types.AwsCloudMapServiceDiscovery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.AwsCloudMapServiceDiscovery where

import Amazonka.AppMesh.Types.AwsCloudMapInstanceAttribute
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the Cloud Map service discovery information
-- for your virtual node.
--
-- Cloud Map is not available in the eu-south-1 Region.
--
-- /See:/ 'newAwsCloudMapServiceDiscovery' smart constructor.
data AwsCloudMapServiceDiscovery = AwsCloudMapServiceDiscovery'
  { -- | A string map that contains attributes with values that you can use to
    -- filter instances by any custom attribute that you specified when you
    -- registered the instance. Only instances that match all of the specified
    -- key\/value pairs will be returned.
    attributes :: Prelude.Maybe [AwsCloudMapInstanceAttribute],
    -- | The name of the Cloud Map namespace to use.
    namespaceName :: Prelude.Text,
    -- | The name of the Cloud Map service to use.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudMapServiceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'awsCloudMapServiceDiscovery_attributes' - A string map that contains attributes with values that you can use to
-- filter instances by any custom attribute that you specified when you
-- registered the instance. Only instances that match all of the specified
-- key\/value pairs will be returned.
--
-- 'namespaceName', 'awsCloudMapServiceDiscovery_namespaceName' - The name of the Cloud Map namespace to use.
--
-- 'serviceName', 'awsCloudMapServiceDiscovery_serviceName' - The name of the Cloud Map service to use.
newAwsCloudMapServiceDiscovery ::
  -- | 'namespaceName'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  AwsCloudMapServiceDiscovery
newAwsCloudMapServiceDiscovery
  pNamespaceName_
  pServiceName_ =
    AwsCloudMapServiceDiscovery'
      { attributes =
          Prelude.Nothing,
        namespaceName = pNamespaceName_,
        serviceName = pServiceName_
      }

-- | A string map that contains attributes with values that you can use to
-- filter instances by any custom attribute that you specified when you
-- registered the instance. Only instances that match all of the specified
-- key\/value pairs will be returned.
awsCloudMapServiceDiscovery_attributes :: Lens.Lens' AwsCloudMapServiceDiscovery (Prelude.Maybe [AwsCloudMapInstanceAttribute])
awsCloudMapServiceDiscovery_attributes = Lens.lens (\AwsCloudMapServiceDiscovery' {attributes} -> attributes) (\s@AwsCloudMapServiceDiscovery' {} a -> s {attributes = a} :: AwsCloudMapServiceDiscovery) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Cloud Map namespace to use.
awsCloudMapServiceDiscovery_namespaceName :: Lens.Lens' AwsCloudMapServiceDiscovery Prelude.Text
awsCloudMapServiceDiscovery_namespaceName = Lens.lens (\AwsCloudMapServiceDiscovery' {namespaceName} -> namespaceName) (\s@AwsCloudMapServiceDiscovery' {} a -> s {namespaceName = a} :: AwsCloudMapServiceDiscovery)

-- | The name of the Cloud Map service to use.
awsCloudMapServiceDiscovery_serviceName :: Lens.Lens' AwsCloudMapServiceDiscovery Prelude.Text
awsCloudMapServiceDiscovery_serviceName = Lens.lens (\AwsCloudMapServiceDiscovery' {serviceName} -> serviceName) (\s@AwsCloudMapServiceDiscovery' {} a -> s {serviceName = a} :: AwsCloudMapServiceDiscovery)

instance Core.FromJSON AwsCloudMapServiceDiscovery where
  parseJSON =
    Core.withObject
      "AwsCloudMapServiceDiscovery"
      ( \x ->
          AwsCloudMapServiceDiscovery'
            Prelude.<$> (x Core..:? "attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "namespaceName")
            Prelude.<*> (x Core..: "serviceName")
      )

instance Prelude.Hashable AwsCloudMapServiceDiscovery where
  hashWithSalt salt' AwsCloudMapServiceDiscovery' {..} =
    salt' `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData AwsCloudMapServiceDiscovery where
  rnf AwsCloudMapServiceDiscovery' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf namespaceName

instance Core.ToJSON AwsCloudMapServiceDiscovery where
  toJSON AwsCloudMapServiceDiscovery' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("attributes" Core..=) Prelude.<$> attributes,
            Prelude.Just ("namespaceName" Core..= namespaceName),
            Prelude.Just ("serviceName" Core..= serviceName)
          ]
      )
