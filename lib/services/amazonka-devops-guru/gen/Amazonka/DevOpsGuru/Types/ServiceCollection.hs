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
-- Module      : Amazonka.DevOpsGuru.Types.ServiceCollection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ServiceCollection where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.ServiceName
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A collection of the names of AWS services.
--
-- /See:/ 'newServiceCollection' smart constructor.
data ServiceCollection = ServiceCollection'
  { -- | An array of strings that each specifies the name of an AWS service.
    serviceNames :: Prelude.Maybe [ServiceName]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceNames', 'serviceCollection_serviceNames' - An array of strings that each specifies the name of an AWS service.
newServiceCollection ::
  ServiceCollection
newServiceCollection =
  ServiceCollection' {serviceNames = Prelude.Nothing}

-- | An array of strings that each specifies the name of an AWS service.
serviceCollection_serviceNames :: Lens.Lens' ServiceCollection (Prelude.Maybe [ServiceName])
serviceCollection_serviceNames = Lens.lens (\ServiceCollection' {serviceNames} -> serviceNames) (\s@ServiceCollection' {} a -> s {serviceNames = a} :: ServiceCollection) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ServiceCollection where
  parseJSON =
    Core.withObject
      "ServiceCollection"
      ( \x ->
          ServiceCollection'
            Prelude.<$> (x Core..:? "ServiceNames" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ServiceCollection where
  hashWithSalt salt' ServiceCollection' {..} =
    salt' `Prelude.hashWithSalt` serviceNames

instance Prelude.NFData ServiceCollection where
  rnf ServiceCollection' {..} = Prelude.rnf serviceNames

instance Core.ToJSON ServiceCollection where
  toJSON ServiceCollection' {..} =
    Core.object
      ( Prelude.catMaybes
          [("ServiceNames" Core..=) Prelude.<$> serviceNames]
      )
