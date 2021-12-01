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
-- Module      : Amazonka.ImageBuilder.Types.Component
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.Component where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types.ComponentParameterDetail
import Amazonka.ImageBuilder.Types.ComponentState
import Amazonka.ImageBuilder.Types.ComponentType
import Amazonka.ImageBuilder.Types.Platform
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A detailed view of a component.
--
-- /See:/ 'newComponent' smart constructor.
data Component = Component'
  { -- | Describes the current status of the component. This is used for
    -- components that are no longer active.
    state :: Prelude.Maybe ComponentState,
    -- | The platform of the component.
    platform :: Prelude.Maybe Platform,
    -- | The Amazon Resource Name (ARN) of the component.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The data of the component.
    data' :: Prelude.Maybe Prelude.Text,
    -- | The operating system (OS) version supported by the component. If the OS
    -- information is available, a prefix match is performed against the base
    -- image OS version during image recipe creation.
    supportedOsVersions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The encryption status of the component.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The owner of the component.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The date that the component was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The name of the component.
    name :: Prelude.Maybe Prelude.Text,
    -- | The KMS key identifier used to encrypt the component.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The version of the component.
    version :: Prelude.Maybe Prelude.Text,
    -- | Contains parameter details for each of the parameters that are defined
    -- for the component.
    parameters :: Prelude.Maybe [ComponentParameterDetail],
    -- | The change description of the component.
    changeDescription :: Prelude.Maybe Prelude.Text,
    -- | The type of the component denotes whether the component is used to build
    -- the image or only to test it.
    type' :: Prelude.Maybe ComponentType,
    -- | The description of the component.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the component.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Component' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'component_state' - Describes the current status of the component. This is used for
-- components that are no longer active.
--
-- 'platform', 'component_platform' - The platform of the component.
--
-- 'arn', 'component_arn' - The Amazon Resource Name (ARN) of the component.
--
-- 'data'', 'component_data' - The data of the component.
--
-- 'supportedOsVersions', 'component_supportedOsVersions' - The operating system (OS) version supported by the component. If the OS
-- information is available, a prefix match is performed against the base
-- image OS version during image recipe creation.
--
-- 'encrypted', 'component_encrypted' - The encryption status of the component.
--
-- 'owner', 'component_owner' - The owner of the component.
--
-- 'dateCreated', 'component_dateCreated' - The date that the component was created.
--
-- 'name', 'component_name' - The name of the component.
--
-- 'kmsKeyId', 'component_kmsKeyId' - The KMS key identifier used to encrypt the component.
--
-- 'version', 'component_version' - The version of the component.
--
-- 'parameters', 'component_parameters' - Contains parameter details for each of the parameters that are defined
-- for the component.
--
-- 'changeDescription', 'component_changeDescription' - The change description of the component.
--
-- 'type'', 'component_type' - The type of the component denotes whether the component is used to build
-- the image or only to test it.
--
-- 'description', 'component_description' - The description of the component.
--
-- 'tags', 'component_tags' - The tags associated with the component.
newComponent ::
  Component
newComponent =
  Component'
    { state = Prelude.Nothing,
      platform = Prelude.Nothing,
      arn = Prelude.Nothing,
      data' = Prelude.Nothing,
      supportedOsVersions = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      owner = Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      name = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      version = Prelude.Nothing,
      parameters = Prelude.Nothing,
      changeDescription = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Describes the current status of the component. This is used for
-- components that are no longer active.
component_state :: Lens.Lens' Component (Prelude.Maybe ComponentState)
component_state = Lens.lens (\Component' {state} -> state) (\s@Component' {} a -> s {state = a} :: Component)

-- | The platform of the component.
component_platform :: Lens.Lens' Component (Prelude.Maybe Platform)
component_platform = Lens.lens (\Component' {platform} -> platform) (\s@Component' {} a -> s {platform = a} :: Component)

-- | The Amazon Resource Name (ARN) of the component.
component_arn :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_arn = Lens.lens (\Component' {arn} -> arn) (\s@Component' {} a -> s {arn = a} :: Component)

-- | The data of the component.
component_data :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_data = Lens.lens (\Component' {data'} -> data') (\s@Component' {} a -> s {data' = a} :: Component)

-- | The operating system (OS) version supported by the component. If the OS
-- information is available, a prefix match is performed against the base
-- image OS version during image recipe creation.
component_supportedOsVersions :: Lens.Lens' Component (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
component_supportedOsVersions = Lens.lens (\Component' {supportedOsVersions} -> supportedOsVersions) (\s@Component' {} a -> s {supportedOsVersions = a} :: Component) Prelude.. Lens.mapping Lens.coerced

-- | The encryption status of the component.
component_encrypted :: Lens.Lens' Component (Prelude.Maybe Prelude.Bool)
component_encrypted = Lens.lens (\Component' {encrypted} -> encrypted) (\s@Component' {} a -> s {encrypted = a} :: Component)

-- | The owner of the component.
component_owner :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_owner = Lens.lens (\Component' {owner} -> owner) (\s@Component' {} a -> s {owner = a} :: Component)

-- | The date that the component was created.
component_dateCreated :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_dateCreated = Lens.lens (\Component' {dateCreated} -> dateCreated) (\s@Component' {} a -> s {dateCreated = a} :: Component)

-- | The name of the component.
component_name :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_name = Lens.lens (\Component' {name} -> name) (\s@Component' {} a -> s {name = a} :: Component)

-- | The KMS key identifier used to encrypt the component.
component_kmsKeyId :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_kmsKeyId = Lens.lens (\Component' {kmsKeyId} -> kmsKeyId) (\s@Component' {} a -> s {kmsKeyId = a} :: Component)

-- | The version of the component.
component_version :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_version = Lens.lens (\Component' {version} -> version) (\s@Component' {} a -> s {version = a} :: Component)

-- | Contains parameter details for each of the parameters that are defined
-- for the component.
component_parameters :: Lens.Lens' Component (Prelude.Maybe [ComponentParameterDetail])
component_parameters = Lens.lens (\Component' {parameters} -> parameters) (\s@Component' {} a -> s {parameters = a} :: Component) Prelude.. Lens.mapping Lens.coerced

-- | The change description of the component.
component_changeDescription :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_changeDescription = Lens.lens (\Component' {changeDescription} -> changeDescription) (\s@Component' {} a -> s {changeDescription = a} :: Component)

-- | The type of the component denotes whether the component is used to build
-- the image or only to test it.
component_type :: Lens.Lens' Component (Prelude.Maybe ComponentType)
component_type = Lens.lens (\Component' {type'} -> type') (\s@Component' {} a -> s {type' = a} :: Component)

-- | The description of the component.
component_description :: Lens.Lens' Component (Prelude.Maybe Prelude.Text)
component_description = Lens.lens (\Component' {description} -> description) (\s@Component' {} a -> s {description = a} :: Component)

-- | The tags associated with the component.
component_tags :: Lens.Lens' Component (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
component_tags = Lens.lens (\Component' {tags} -> tags) (\s@Component' {} a -> s {tags = a} :: Component) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Component where
  parseJSON =
    Core.withObject
      "Component"
      ( \x ->
          Component'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "platform")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "data")
            Prelude.<*> (x Core..:? "supportedOsVersions")
            Prelude.<*> (x Core..:? "encrypted")
            Prelude.<*> (x Core..:? "owner")
            Prelude.<*> (x Core..:? "dateCreated")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "kmsKeyId")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "changeDescription")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Component where
  hashWithSalt salt' Component' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` changeDescription
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dateCreated
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` supportedOsVersions
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` state

instance Prelude.NFData Component where
  rnf Component' {..} =
    Prelude.rnf state `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf changeDescription
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf supportedOsVersions
      `Prelude.seq` Prelude.rnf data'
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf platform
