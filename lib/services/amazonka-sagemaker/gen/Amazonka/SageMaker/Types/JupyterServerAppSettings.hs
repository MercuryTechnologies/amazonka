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
-- Module      : Amazonka.SageMaker.Types.JupyterServerAppSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.JupyterServerAppSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ResourceSpec

-- | The JupyterServer app settings.
--
-- /See:/ 'newJupyterServerAppSettings' smart constructor.
data JupyterServerAppSettings = JupyterServerAppSettings'
  { -- | The default instance type and the Amazon Resource Name (ARN) of the
    -- default SageMaker image used by the JupyterServer app.
    defaultResourceSpec :: Prelude.Maybe ResourceSpec,
    -- | The Amazon Resource Name (ARN) of the Lifecycle Configurations attached
    -- to the JupyterServerApp.
    lifecycleConfigArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JupyterServerAppSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultResourceSpec', 'jupyterServerAppSettings_defaultResourceSpec' - The default instance type and the Amazon Resource Name (ARN) of the
-- default SageMaker image used by the JupyterServer app.
--
-- 'lifecycleConfigArns', 'jupyterServerAppSettings_lifecycleConfigArns' - The Amazon Resource Name (ARN) of the Lifecycle Configurations attached
-- to the JupyterServerApp.
newJupyterServerAppSettings ::
  JupyterServerAppSettings
newJupyterServerAppSettings =
  JupyterServerAppSettings'
    { defaultResourceSpec =
        Prelude.Nothing,
      lifecycleConfigArns = Prelude.Nothing
    }

-- | The default instance type and the Amazon Resource Name (ARN) of the
-- default SageMaker image used by the JupyterServer app.
jupyterServerAppSettings_defaultResourceSpec :: Lens.Lens' JupyterServerAppSettings (Prelude.Maybe ResourceSpec)
jupyterServerAppSettings_defaultResourceSpec = Lens.lens (\JupyterServerAppSettings' {defaultResourceSpec} -> defaultResourceSpec) (\s@JupyterServerAppSettings' {} a -> s {defaultResourceSpec = a} :: JupyterServerAppSettings)

-- | The Amazon Resource Name (ARN) of the Lifecycle Configurations attached
-- to the JupyterServerApp.
jupyterServerAppSettings_lifecycleConfigArns :: Lens.Lens' JupyterServerAppSettings (Prelude.Maybe [Prelude.Text])
jupyterServerAppSettings_lifecycleConfigArns = Lens.lens (\JupyterServerAppSettings' {lifecycleConfigArns} -> lifecycleConfigArns) (\s@JupyterServerAppSettings' {} a -> s {lifecycleConfigArns = a} :: JupyterServerAppSettings) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON JupyterServerAppSettings where
  parseJSON =
    Core.withObject
      "JupyterServerAppSettings"
      ( \x ->
          JupyterServerAppSettings'
            Prelude.<$> (x Core..:? "DefaultResourceSpec")
            Prelude.<*> ( x Core..:? "LifecycleConfigArns"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable JupyterServerAppSettings where
  hashWithSalt salt' JupyterServerAppSettings' {..} =
    salt' `Prelude.hashWithSalt` lifecycleConfigArns
      `Prelude.hashWithSalt` defaultResourceSpec

instance Prelude.NFData JupyterServerAppSettings where
  rnf JupyterServerAppSettings' {..} =
    Prelude.rnf defaultResourceSpec
      `Prelude.seq` Prelude.rnf lifecycleConfigArns

instance Core.ToJSON JupyterServerAppSettings where
  toJSON JupyterServerAppSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DefaultResourceSpec" Core..=)
              Prelude.<$> defaultResourceSpec,
            ("LifecycleConfigArns" Core..=)
              Prelude.<$> lifecycleConfigArns
          ]
      )
