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
-- Module      : Amazonka.RAM.Types.ServiceNameAndResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ServiceNameAndResourceType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the shareable resource types and the Amazon Web
-- Services services to which they belong.
--
-- /See:/ 'newServiceNameAndResourceType' smart constructor.
data ServiceNameAndResourceType = ServiceNameAndResourceType'
  { -- | The shareable resource types.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Web Services services to which the resources
    -- belong.
    serviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNameAndResourceType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'serviceNameAndResourceType_resourceType' - The shareable resource types.
--
-- 'serviceName', 'serviceNameAndResourceType_serviceName' - The name of the Amazon Web Services services to which the resources
-- belong.
newServiceNameAndResourceType ::
  ServiceNameAndResourceType
newServiceNameAndResourceType =
  ServiceNameAndResourceType'
    { resourceType =
        Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | The shareable resource types.
serviceNameAndResourceType_resourceType :: Lens.Lens' ServiceNameAndResourceType (Prelude.Maybe Prelude.Text)
serviceNameAndResourceType_resourceType = Lens.lens (\ServiceNameAndResourceType' {resourceType} -> resourceType) (\s@ServiceNameAndResourceType' {} a -> s {resourceType = a} :: ServiceNameAndResourceType)

-- | The name of the Amazon Web Services services to which the resources
-- belong.
serviceNameAndResourceType_serviceName :: Lens.Lens' ServiceNameAndResourceType (Prelude.Maybe Prelude.Text)
serviceNameAndResourceType_serviceName = Lens.lens (\ServiceNameAndResourceType' {serviceName} -> serviceName) (\s@ServiceNameAndResourceType' {} a -> s {serviceName = a} :: ServiceNameAndResourceType)

instance Core.FromJSON ServiceNameAndResourceType where
  parseJSON =
    Core.withObject
      "ServiceNameAndResourceType"
      ( \x ->
          ServiceNameAndResourceType'
            Prelude.<$> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "serviceName")
      )

instance Prelude.Hashable ServiceNameAndResourceType where
  hashWithSalt salt' ServiceNameAndResourceType' {..} =
    salt' `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ServiceNameAndResourceType where
  rnf ServiceNameAndResourceType' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf serviceName
