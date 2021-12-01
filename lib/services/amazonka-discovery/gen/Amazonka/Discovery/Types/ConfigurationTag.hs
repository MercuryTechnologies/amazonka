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
-- Module      : Amazonka.Discovery.Types.ConfigurationTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.ConfigurationTag where

import qualified Amazonka.Core as Core
import Amazonka.Discovery.Types.ConfigurationItemType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Tags for a configuration item. Tags are metadata that help you
-- categorize IT assets.
--
-- /See:/ 'newConfigurationTag' smart constructor.
data ConfigurationTag = ConfigurationTag'
  { -- | The time the configuration tag was created in Coordinated Universal Time
    -- (UTC).
    timeOfCreation :: Prelude.Maybe Core.POSIX,
    -- | The configuration ID for the item to tag. You can specify a list of keys
    -- and values.
    configurationId :: Prelude.Maybe Prelude.Text,
    -- | A type of IT asset to tag.
    configurationType :: Prelude.Maybe ConfigurationItemType,
    -- | A value on which to filter. For example /key = serverType/ and /value =
    -- web server/.
    value :: Prelude.Maybe Prelude.Text,
    -- | A type of tag on which to filter. For example, /serverType/.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeOfCreation', 'configurationTag_timeOfCreation' - The time the configuration tag was created in Coordinated Universal Time
-- (UTC).
--
-- 'configurationId', 'configurationTag_configurationId' - The configuration ID for the item to tag. You can specify a list of keys
-- and values.
--
-- 'configurationType', 'configurationTag_configurationType' - A type of IT asset to tag.
--
-- 'value', 'configurationTag_value' - A value on which to filter. For example /key = serverType/ and /value =
-- web server/.
--
-- 'key', 'configurationTag_key' - A type of tag on which to filter. For example, /serverType/.
newConfigurationTag ::
  ConfigurationTag
newConfigurationTag =
  ConfigurationTag'
    { timeOfCreation = Prelude.Nothing,
      configurationId = Prelude.Nothing,
      configurationType = Prelude.Nothing,
      value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The time the configuration tag was created in Coordinated Universal Time
-- (UTC).
configurationTag_timeOfCreation :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.UTCTime)
configurationTag_timeOfCreation = Lens.lens (\ConfigurationTag' {timeOfCreation} -> timeOfCreation) (\s@ConfigurationTag' {} a -> s {timeOfCreation = a} :: ConfigurationTag) Prelude.. Lens.mapping Core._Time

-- | The configuration ID for the item to tag. You can specify a list of keys
-- and values.
configurationTag_configurationId :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.Text)
configurationTag_configurationId = Lens.lens (\ConfigurationTag' {configurationId} -> configurationId) (\s@ConfigurationTag' {} a -> s {configurationId = a} :: ConfigurationTag)

-- | A type of IT asset to tag.
configurationTag_configurationType :: Lens.Lens' ConfigurationTag (Prelude.Maybe ConfigurationItemType)
configurationTag_configurationType = Lens.lens (\ConfigurationTag' {configurationType} -> configurationType) (\s@ConfigurationTag' {} a -> s {configurationType = a} :: ConfigurationTag)

-- | A value on which to filter. For example /key = serverType/ and /value =
-- web server/.
configurationTag_value :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.Text)
configurationTag_value = Lens.lens (\ConfigurationTag' {value} -> value) (\s@ConfigurationTag' {} a -> s {value = a} :: ConfigurationTag)

-- | A type of tag on which to filter. For example, /serverType/.
configurationTag_key :: Lens.Lens' ConfigurationTag (Prelude.Maybe Prelude.Text)
configurationTag_key = Lens.lens (\ConfigurationTag' {key} -> key) (\s@ConfigurationTag' {} a -> s {key = a} :: ConfigurationTag)

instance Core.FromJSON ConfigurationTag where
  parseJSON =
    Core.withObject
      "ConfigurationTag"
      ( \x ->
          ConfigurationTag'
            Prelude.<$> (x Core..:? "timeOfCreation")
            Prelude.<*> (x Core..:? "configurationId")
            Prelude.<*> (x Core..:? "configurationType")
            Prelude.<*> (x Core..:? "value")
            Prelude.<*> (x Core..:? "key")
      )

instance Prelude.Hashable ConfigurationTag where
  hashWithSalt salt' ConfigurationTag' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` configurationType
      `Prelude.hashWithSalt` configurationId
      `Prelude.hashWithSalt` timeOfCreation

instance Prelude.NFData ConfigurationTag where
  rnf ConfigurationTag' {..} =
    Prelude.rnf timeOfCreation
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf configurationType
      `Prelude.seq` Prelude.rnf configurationId
