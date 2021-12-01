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
-- Module      : Amazonka.APIGateway.Types.SdkType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.SdkType where

import Amazonka.APIGateway.Types.SdkConfigurationProperty
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A type of SDK that API Gateway can generate.
--
-- /See:/ 'newSdkType' smart constructor.
data SdkType = SdkType'
  { -- | The user-friendly name of an SdkType instance.
    friendlyName :: Prelude.Maybe Prelude.Text,
    -- | A list of configuration properties of an SdkType.
    configurationProperties :: Prelude.Maybe [SdkConfigurationProperty],
    -- | The identifier of an SdkType instance.
    id :: Prelude.Maybe Prelude.Text,
    -- | The description of an SdkType.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SdkType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'friendlyName', 'sdkType_friendlyName' - The user-friendly name of an SdkType instance.
--
-- 'configurationProperties', 'sdkType_configurationProperties' - A list of configuration properties of an SdkType.
--
-- 'id', 'sdkType_id' - The identifier of an SdkType instance.
--
-- 'description', 'sdkType_description' - The description of an SdkType.
newSdkType ::
  SdkType
newSdkType =
  SdkType'
    { friendlyName = Prelude.Nothing,
      configurationProperties = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The user-friendly name of an SdkType instance.
sdkType_friendlyName :: Lens.Lens' SdkType (Prelude.Maybe Prelude.Text)
sdkType_friendlyName = Lens.lens (\SdkType' {friendlyName} -> friendlyName) (\s@SdkType' {} a -> s {friendlyName = a} :: SdkType)

-- | A list of configuration properties of an SdkType.
sdkType_configurationProperties :: Lens.Lens' SdkType (Prelude.Maybe [SdkConfigurationProperty])
sdkType_configurationProperties = Lens.lens (\SdkType' {configurationProperties} -> configurationProperties) (\s@SdkType' {} a -> s {configurationProperties = a} :: SdkType) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of an SdkType instance.
sdkType_id :: Lens.Lens' SdkType (Prelude.Maybe Prelude.Text)
sdkType_id = Lens.lens (\SdkType' {id} -> id) (\s@SdkType' {} a -> s {id = a} :: SdkType)

-- | The description of an SdkType.
sdkType_description :: Lens.Lens' SdkType (Prelude.Maybe Prelude.Text)
sdkType_description = Lens.lens (\SdkType' {description} -> description) (\s@SdkType' {} a -> s {description = a} :: SdkType)

instance Core.FromJSON SdkType where
  parseJSON =
    Core.withObject
      "SdkType"
      ( \x ->
          SdkType'
            Prelude.<$> (x Core..:? "friendlyName")
            Prelude.<*> ( x Core..:? "configurationProperties"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable SdkType where
  hashWithSalt salt' SdkType' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` configurationProperties
      `Prelude.hashWithSalt` friendlyName

instance Prelude.NFData SdkType where
  rnf SdkType' {..} =
    Prelude.rnf friendlyName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf configurationProperties
